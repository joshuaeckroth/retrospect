(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est
          update-est goto-start-of-time print-est goto-ep ep-path goto-cycle]])
  (:use [retrospect.reason.abduction.workspace :only
         [get-no-explainers get-unexplained new-hyp init-workspace calc-doubt
          explain add-kb add-observation add lookup-hyp explainers
          update-kb explain update-hypotheses add-sensor-hyps conflicts?
          reject-many rejection-reason]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(declare metareason)

(defn find-problem-cases
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (set (map #(lookup-hyp workspace %) (get-no-explainers workspace)))))

(defn metareasoning-activated?
  [est]
  (and (not= "none" (:Metareasoning params))
       (not-empty (find-problem-cases est))))

(defn reason
  [est time-prev time-now sensors & opts]
  (let [ws (:workspace (cur-ep est))
        ws-sensors (binding [reason-log (ref '())]
                     (assoc (if sensors
                              (update-hypotheses
                               (add-sensor-hyps ws time-prev time-now sensors
                                                (:cycle (cur-ep est)))
                               time-now)
                              ws)
                       :log @reason-log))
        est-ws (new-child-ep (update-est est (assoc (cur-ep est)
                                               :workspace ws-sensors :time time-now)))]
    (loop [est est-ws]
      (let [ws (binding [reason-log (ref '())]
                 (log "Explaining at cycle" (:cycle (cur-ep est)))
                 (assoc (explain (:workspace (cur-ep est)) (:cycle (cur-ep est)))
                   :log @reason-log))
            est-new (new-child-ep (update-est est (assoc (cur-ep est) :workspace ws)))
            meta? (and (not-any? #{:no-metareason} opts)
                       (metareasoning-activated? est-new))
            est-meta (if (not meta?) est-new
                         (metareason est-new time-prev time-now sensors))]
        ;; if something was accepted last or we did metareasoning, repeat
        (if (or meta? (:best (:accrej (:workspace (cur-ep est-meta)))))
          (recur est-meta) est-meta)))))

(defn meta-apply-and-evaluate
  [est est-new time-now sensors]
  (let [reason-est (reason est-new (:time (cur-ep est-new)) time-now sensors :no-metareason)]
    {:est-old (goto-ep reason-est (:id (cur-ep est)))
     :est-new reason-est}))

(comment
  (defn abductive-metareason
    [truedata est time-prev time-now sensors]
    (let [workspace (:workspace (cur-ep est))
          ws-depth (workspace-depth workspace)
          noexp (map (partial lookup-hyp workspace) (get-no-explainers workspace))
          noexp-hyps (make-noexp-hyps noexp)
          meta-hyps (reverse (sort-by :apriori
                                      (filter #(>= (:apriori %)
                                             (/ (double (:MetaMinApriori params)) 100.0))
                                         (make-meta-hyps workspace noexp-hyps
                                                         ws-depth time-now))))]
      (loop [est-meta est
             hyps meta-hyps
             attempted-depths #{}
             attempts 0]
        (cond (or (empty? hyps) (= attempts (:MetaBatchAttempts params)))
              ;; give up, force noexp hyps to be ignored
              {:est (force-resolve-trigger
                     truedata (goto-ep est-meta (:id (cur-ep est)))
                     time-prev time-now sensors)
               :considered? true
               :accepted-branch? false}
              ;; if we haven't attempted anything this deep
              (every? #(> (:depth (first hyps)) %) attempted-depths)
              (let [result (apply-resolutions [(first hyps)]
                                              est-meta time-prev time-now sensors)]
                (if (:accepted-branch? result) result
                    (recur (:est result) (rest hyps)
                           (conj attempted-depths (:depth (first hyps))) (inc attempts))))
              ;; else, already have attempted something this deep or
              ;; more; move on to next hyp
              :else
              (recur est-meta (rest hyps) attempted-depths attempts))))))

(defn meta-batch
  [n problem-caes est _ time-now sensors]
  (let [time (if n (- time-now n) 0)
        new-est (new-branch-ep est (cur-ep (goto-start-of-time est time)))]
    (meta-apply-and-evaluate est new-est time-now sensors)))

(defn meta-batch-weakest
  [problem-cases est _ time-now sensors]
  (let [doubts-cycles (reverse (sort-by first
                                        (filter first
                                           (map (fn [ep]
                                                [(calc-doubt (:workspace ep)) (:cycle ep)])
                                              (ep-path est)))))
        highest-doubt (ffirst doubts-cycles)
        prior-cycle (dec (second (first (sort-by second (filter #(= highest-doubt (first %))
                                                           doubts-cycles)))))
        new-est (new-branch-ep est (cur-ep (goto-cycle est prior-cycle)))]
    (meta-apply-and-evaluate est new-est time-now sensors)))

(defn meta-lower-minapriori
  [problem-cases est time-prev time-now sensors]
  (when (not= 0 (:MinApriori params))
    (let [new-est (new-branch-ep est (cur-ep (goto-start-of-time est time-now)))]
      ;; drop min-apriori to 0
      (binding [params (assoc params :MinApriori 0)]
        (meta-apply-and-evaluate est new-est time-now sensors)))))

(defn preemptively-reject
  [est hyp]
  (let [ep (cur-ep est)
        ws-rejected (reject-many (:workspace ep) [hyp] :preemptive (:cycle ep))
        ep-rejected (assoc ep :workspace ws-rejected)]
    (update-est est ep-rejected)))

(defn find-rejected-explainers
  "For one problem case (noexp), there is a set of explainers (which
   may be empty). All of these (if any exist) must have been rejected
   at some time. (If they just weren't accepted due to a
   delta-threshold, they would not appear as noexp.) They were each
   rejected when something else was accepted. Maybe some of those
   accepted hyps were not essential, and had alternatives, some or all
   of which do not conflict with the explainers of the noexp. This
   meta-strategy chooses to branch back to the acceptance point of the
   accepted hyp (which rejected an explainer of the noexp) that had
   the lowest normalized-delta, and pre-emptively reject it, thus
   favoring its next-best. Only makes sense when the problem case
   given to this function is a noexp case."
  [est problem-case]
  (let [expl (set (explainers (:workspace (cur-ep est)) problem-case))
        expl-rejected-conflicts (filter (fn [h] (= :conflict (rejection-reason (:workspace (cur-ep est)) h)))
                                   expl)
        ;; which ep-states rejected these expl and are
        ;; not states in which what was accepted was
        ;; essential?
        ep-rejs (filter (fn [ep] (and (some (set (map :contents expl-rejected-conflicts))
                                   (map :contents (:rej (:accrej (:workspace ep)))))
                                (:nbest (:accrej (:workspace ep)))
                                ;; restrict to somewhat "ambiguous" decisions
                                (<= (:delta (:accrej (:workspace ep))) 0.5)))
                   (ep-path est))
        bad-bests (reverse
                   (sort-by first (set
                                   (filter (fn [[delta _ best]] (and delta best))
                                      (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                                  (:cycle ep)
                                                  (get-in ep [:workspace :accrej :best])])
                                         ep-rejs)))))
        [_ cycle best] (first bad-bests)]
    (comment
      (println "expl" expl)
      (println "expl-rejected-conflicts" expl-rejected-conflicts)
      (println "ep-rejs:" (map str ep-rejs) (map #(:nbest (:accrej (:workspace %))) ep-rejs))
      (println "bad-bests" bad-bests))
    (when cycle
      (let [new-est (new-branch-ep est (cur-ep (goto-cycle est (dec cycle))))]
        (preemptively-reject new-est best)))))

(defn meta-reject-conflicting
  [problem-cases est time-prev time-now sensors]
  (let [est-rej (find-rejected-explainers est (first problem-cases))]
    (when est-rej
      (meta-apply-and-evaluate est est-rej time-now sensors))))

(defn action-batch
  [ep est]
  [(new-branch-ep est ep)
   params])

(defn action-flip-choice
  [ep est]
  [(new-branch-ep est ep)
   params])

(defn action-lower-minapriori
  [time-now est]
  [(new-branch-ep est (cur-ep (goto-start-of-time est time-now)))
   (assoc params :MinApriori 0)])

(defn action-ignore
  [hyp est]
  (let [ws-old (:workspace (cur-ep est))
        new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-ignored (reject-many (:workspace new-ep) [hyp] :ignoring (:cycle new-ep))]
    [(update-est new-est (assoc new-ep :workspace ws-ignored))
     params]))

(defn action-preemptively-reject
  [hyp cycle est]
  (let [new-est (new-branch-ep est (cur-ep (goto-cycle est (- cycle 2))))
        ep (cur-ep new-est)
        ws-rejected (reject-many (:workspace ep) [hyp] :preemptive (:cycle ep))
        ep-rejected (assoc ep :workspace ws-rejected)]
    (comment (println "rejected" hyp "at cycle" cycle))
    [(update-est new-est ep-rejected)
     params]))

(defn meta-hyp-conflicts?
  [hyp1 hyp2]
  (and (not= hyp1 hyp2)
       (not-empty (set/intersection (set (:explains hyp1)) (set (:explains hyp2))))))

(comment
  (let [doubts-times
        (reverse (sort-by first
                          (filter first
                             (map (fn [ep]
                                  [(calc-doubt (:workspace ep)) (:time ep)])
                                (filter #((set (range 2 (dec time-now))) (:time %))
                                   (ep-path est))))))
        highest-doubt (ffirst doubts-times)
        prior-weakest (when highest-doubt
                        (dec (second (first (sort-by second
                                                     (filter #(= highest-doubt (first %))
                                                        doubts-times))))))]))

(defn make-meta-hyps
  "Create explanations, and associated actions, for problem-cases."
  [problem-cases est time-prev time-now sensors]
  (let [cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) problem-cases))
        eps (ep-path est)]
    (reverse
     (sort-by :cycle
              (concat
               ;; order dependency among the observations
               (if (not= 0 time-prev)
                 (let [expl-rejected-conflicts (filter (fn [h] (= :conflict (rejection-reason cur-ws h)))
                                                  expl)
                       ep-rejs (filter (fn [ep] (some (set (map :contents expl-rejected-conflicts))
                                             (map :contents (:rej (:accrej (:workspace ep))))))
                                  (ep-path est))
                       t (if (empty? ep-rejs) (dec time-now) (dec (apply min (map :time ep-rejs))))
                       ep (cur-ep (goto-start-of-time est t))]
                   (comment (println time-now t "count ep-rejs" (count ep-rejs) expl-rejected-conflicts))
                   [(new-hyp "OrderDep" :order-dep :order-dep
                             1.0 false meta-hyp-conflicts? []
                             (format "Order dependency at %s" (str ep))
                             (format "Order dependency at %s" (str ep))
                             {:action (partial action-batch ep) :cycle (:cycle ep)})])
                 ;; time-prev == 0, so this is a "static" case or we have not
                 ;; done much reasoning yet
                 [])
               ;; correct explainer(s) were rejected due to conflicts; need to
               ;; consider the various possibilities of rejected explainers and
               ;; no-explainers combinations
               (let [expl-rejected-conflicts (filter (fn [h] (= :conflict (rejection-reason cur-ws h)))
                                                expl)
                     ;; which ep-states rejected these expl and are
                     ;; not states in which what was accepted was
                     ;; essential?
                     ep-rejs (filter (fn [ep] (and (some (set (map :contents expl-rejected-conflicts))
                                                (map :contents (:rej (:accrej (:workspace ep)))))
                                             (:nbest (:accrej (:workspace ep)))
                                             ;; restrict to somewhat "ambiguous" decisions
                                             (<= (:delta (:accrej (:workspace ep))) 0.5)))
                                (ep-path est))
                     bad-bests (reverse
                                (sort-by first (set
                                                (filter (fn [[delta _ best]] (and delta best))
                                                   (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                                               (:cycle ep)
                                                               (get-in ep [:workspace :accrej :best])])
                                                      ep-rejs)))))]
                 (comment
                   (println "expl" expl)
                   (println "expl-rejected-conflicts" expl-rejected-conflicts)
                   (println "ep-rejs:" (map str ep-rejs) (map #(:nbest (:accrej (:workspace %))) ep-rejs))
                   (println "bad-bests" bad-bests))
                 (for [[delta cycle hyp] bad-bests]
                   (new-hyp "Rejected" :rejected :conflict
                            1.0 false meta-hyp-conflicts? []
                            (format "%s rejected some explainers" hyp)
                            (format "%s rejected some explainers (cycle %d, delta %.2f"
                               (str hyp) cycle delta)
                            {:action (partial action-preemptively-reject hyp cycle)
                             :cycle cycle})))
               ;; wrong choice at a certain decision (batch + flip)
               (comment
                 (for [ep (filter (comp :nbest :accrej :workspace) eps)]
                   (new-hyp "WrongChoice" :wrong-choice :wrong-choice
                            1.0 false meta-hyp-conflicts? []
                            (format "Wrong choice at %s" (str ep))
                            (format "%s was accepted at %s but %s should be instead."
                               (str (:best (:accrej (:workspace ep)))) (str ep)
                               (str (:nbest (:accrej (:workspace ep)))))
                            {:action (partial action-flip-choice ep)})))
               ;; were some explainers omitted due to high min-apriori?
               (let [expl-rejected-minapriori (filter (fn [h] (= :minapriori (rejection-reason cur-ws h)))
                                                 expl)]
                 (if (not-empty expl-rejected-minapriori)
                   [(new-hyp "TooHighMinApriori" :rejected :minapriori
                             1.0 false meta-hyp-conflicts? []
                             "Explainers rejected due to too-high min-apriori"
                             (format "These explainers were rejected due to too-high min-apriori: %s"
                                (str/join ", " (sort (map str expl-rejected-minapriori))))
                             {:action (partial action-lower-minapriori time-now)
                              :cycle (:cycle (cur-ep (goto-start-of-time est time-now)))})]
                   [])))))))

(defn score-meta-hyps
  [problem-cases meta-hyps est time-prev time-now sensors]
  (loop [est-attempted est
         hyps meta-hyps
         new-hyps []]
    (if (empty? hyps) [(goto-ep est-attempted (:id (cur-ep est))) new-hyps]
        (let [hyp (first hyps)
              [est-new params-new] ((:action hyp) est-attempted)
              result (binding [params params-new]
                       (meta-apply-and-evaluate est-attempted est-new time-now sensors))
              doubt-new (doubt-aggregate (:est-new result))
              problem-cases-new (find-problem-cases (:est-new result))
              resolved-cases (filter (fn [pc] (not ((set (map :contents problem-cases-new))
                                              (:contents pc))))
                                problem-cases)]
          (recur (:est-old result) (rest hyps)
                 (conj new-hyps
                       (assoc hyp :explains (map :contents resolved-cases)
                              :final-ep-id (:id (cur-ep (:est-new result)))
                              :apriori (- 1.0 doubt-new)
                              :desc (format "%s\n\nEp-state start: %s"
                                       (:desc hyp) (str (cur-ep est-new))))))))))

(defn meta-abductive
  [problem-cases est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps problem-cases est time-prev time-now sensors)
        [est-new meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps
                                                    est time-prev time-now sensors)
        meta-est (new-child-ep (init-est (init-workspace)))
        meta-ws (reduce add (reduce #(add-observation %1 %2 0)
                          (:workspace (cur-ep meta-est)) problem-cases)
                   meta-hyps-scored)
        meta-est-reasoned (binding [params (assoc params :MinApriori 0
                                                  :Threshold 0)]
                            (reason (update-est meta-est (assoc (cur-ep meta-est)
                                                           :workspace meta-ws))
                                    0 1 nil :no-metareason))
        meta-ws-reasoned (:workspace (cur-ep meta-est-reasoned))
        ;; take out the "observations"
        meta-accepted (filter (fn [hyp] (not ((set (map :contents problem-cases)) (:contents hyp))))
                         (map #(lookup-hyp meta-ws-reasoned %)
                            (:all (:accepted meta-ws-reasoned))))
        est-new-meta-est (update-est est-new (assoc (cur-ep est)
                                               :meta-est meta-est-reasoned))]
    (comment
      (println "problem cases:" (str/join ", " (map str problem-cases)))
      (println (for [h meta-hyps-scored]
                 (format "%s explains %s\n\n" (str h)
                    (str/join ", " (map str (filter (fn [pc] ((set (:explains h)) (:contents pc)))
                                             problem-cases))))))
      (println "accepted" meta-accepted)
      (println "ep:" (str (cur-ep est-new))))
    ;; optimization: if only one accepted hyp, just go back to the ep
    ;; arrived at by testing/scoring the hyp
    (cond (= 1 (count meta-accepted))
          {:est-old (goto-ep est-new-meta-est (:id (cur-ep est)))
           :est-new (goto-ep est-new-meta-est (:final-ep-id (first meta-accepted)))}
          (not-empty meta-accepted)
          (let [[est-applied params-applied]
                (loop [est-applied est-new-meta-est
                       params-applied params
                       acc meta-accepted]
                  (if (empty? acc) [est-applied params-applied]
                      (let [[est params] ((:action (first acc)) est-applied)]
                        (recur est (merge params-applied params) (rest acc)))))]
            (binding [params params-applied]
              (meta-apply-and-evaluate est-new-meta-est est-applied time-now sensors)))
          :else
          {:est-old (goto-ep est-new-meta-est (:id (cur-ep est))) :est-new est-new-meta-est})))

(defn meta-abductive-recursive
  "Tries one abductive meta-hyp at a time until one works,
   recursively. Noise hyps are skipped; that can be handled
   below (resolve-by-ignoring)."
  [serial? use-doubt? problem-cases est time-prev time-now sensors]
  (comment (println "problem-cases:" problem-cases))
  (comment (println "meta-hyps:" meta-hyps))
  (loop [est-attempt est
         hyps (make-meta-hyps problem-cases est time-prev time-now sensors)
         attempts 0]
    (if (empty? hyps)
      (do (comment (println "no more meta-hyps to try."))
          {:est-old (goto-ep est-attempt (:id (cur-ep est)))
           :est-new est-attempt})
      (do (comment (println "trying" (first hyps) "at" (str (cur-ep est-attempt))))
          (let [[est-next params-applied] ((:action (first hyps)) est-attempt)
                result (binding [params params-applied]
                         (meta-apply-and-evaluate
                          est-attempt est-next time-now sensors))
                problem-cases-new (when result (find-problem-cases (:est-new result)))]
            (comment (println "original problem cases:" problem-cases)
                     (println "new problem cases:" problem-cases-new))
            (cond (or (empty? problem-cases-new) (= attempts 10))
                  (do (comment (println "problem cases now empty"))
                      result)
                  (< (count problem-cases-new)
                     (count problem-cases))
                  (do (comment (println "problem cases reduced; recurring"))
                      (recur (:est-new result)
                             (make-meta-hyps problem-cases-new (:est-new result)
                                             time-prev time-now sensors)
                             (inc attempts)))
                  :else
                  (do (comment (println "problem cases not reduced; trying rest of hyps"))
                      (recur (:est-old result) (rest hyps) (inc attempts)))))))))

(defn resolve-by-ignoring
  [problem-cases est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-old (:workspace (cur-ep est))
        ws-ignored (reject-many (:workspace new-ep) problem-cases :ignoring (:cycle new-ep))
        new-est-ignored (update-est new-est (assoc new-ep :workspace ws-ignored))]
    (reason new-est-ignored time-prev time-now sensors :no-metareason)))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [est time-prev time-now sensors]
  (let [problem-cases (find-problem-cases est)
        m (:Metareasoning params)
        f (cond (= "batchbeg" m)
                (partial meta-batch nil)
                (= "batch1" m)
                (partial meta-batch 1)
                (= "batch2" m)
                (partial meta-batch 2)
                (= "batch3" m)
                (partial meta-batch 3)
                (= "batch4" m)
                (partial meta-batch 4)
                (= "batch5" m)
                (partial meta-batch 5)
                (= "batch-mid" m)
                (partial meta-batch (int (/ (count (set (map :time (ep-path est)))) 2)))
                (= "batch-weakest" m)
                meta-batch-weakest
                (= "lower-minapriori" m)
                meta-lower-minapriori
                (= "reject-conflicting" m)
                meta-reject-conflicting
                (= "abd" m)
                meta-abductive
                (= "abd-serial" m)
                (partial meta-abductive-recursive true false)
                (= "abd-recursive" m)
                (partial meta-abductive-recursive false false)
                (= "abd-recursive-doubt" m)
                (partial meta-abductive-recursive false true)
                (= "ignore" m)
                (constantly {:est-old est :est-new est}))
        result (f problem-cases est time-prev time-now sensors)
        problem-cases-new (when result (find-problem-cases (:est-new result)))]
    (cond (nil? result)
          (resolve-by-ignoring problem-cases est
                               time-prev time-now sensors)
          (empty? problem-cases-new)
          (:est-new result)
          (< (count problem-cases-new) (count problem-cases))
          (resolve-by-ignoring problem-cases-new (:est-new result)
                               time-prev time-now sensors)
          :else
          (resolve-by-ignoring problem-cases (:est-old result)
                               time-prev time-now sensors))))
