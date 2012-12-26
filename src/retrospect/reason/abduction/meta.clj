(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est
          update-est goto-start-of-time print-est goto-ep ep-path goto-cycle]])
  (:use [retrospect.reason.abduction.workspace])
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

(defn est-workspace-child
  [est workspace]
  (new-child-ep (update-est est (assoc (cur-ep est)
                                  :workspace workspace))))

(defn workspace-update-sensors
  [workspace time-prev time-now sensors cycle]
  (binding [reason-log (ref '())]
    (let [ws-sensors (if sensors
                       (add-sensor-hyps workspace time-prev time-now sensors cycle)
                       workspace)
          ws-hyps (if (or sensors (:GetMoreHyps params))
                    (update-hypotheses ws-sensors time-now)
                    ws-sensors)]
      (assoc ws-hyps :log @reason-log))))

(defn workspace-explain
  [workspace cycle time-now]
  (binding [reason-log (ref (:log workspace))]
    (log "Explaining at cycle" cycle)
    (let [ws (explain workspace cycle time-now)]
      (assoc ws :log @reason-log))))

(defn explain-and-advance
  [est time-prev time-now sensors]
  (let [ws (:workspace (cur-ep est))
        cycle (:cycle (cur-ep est))
        ws-sensors (workspace-update-sensors ws time-prev time-now sensors cycle)
        ws-explained (workspace-explain ws-sensors cycle time-now)
        est-result (est-workspace-child est ws-explained)]
    (if (or (and (:GetMoreHyps params)
                 (not= (count (:hyp-ids ws-explained))
                       (count (:hyp-ids ws))))
            (:best (:accrej ws-explained)))
      (recur est-result time-prev time-now sensors)
      est-result)))

(defn compare-est
  [est1 est2]
  (let [ws1 (:workspace (cur-ep est1))
        ws2 (:workspace (cur-ep est2))
        noexp1 (get-no-explainers ws1)
        noexp2 (get-no-explainers ws2)
        unexp1 (get-unexplained ws1)
        unexp2 (get-unexplained ws2)
        doubt1 (doubt-aggregate est1)
        doubt2 (doubt-aggregate est2)]
    (if (and (empty? noexp1) (empty? noexp2))
      (if (and (empty? unexp1) (empty? unexp2))
        (compare doubt1 doubt2)
        (compare (count unexp1) (count unexp2)))
      (compare (count noexp1) (count noexp2)))))

(def seen-workspaces #{})

(defn reason-exhaustive-recursively-cycle
  [est time-now depth]
  (let [cycle (:cycle (cur-ep est))
        workspace (:workspace (cur-ep est))
        ws-outcomes (explain-exhaustive workspace cycle time-now)]
    (comment
      (println (apply str (repeat depth "\t"))
               "depth" depth "count seen-workspaces" (count seen-workspaces)
               "count ws-outcomes:" (count ws-outcomes)))
    (doall
     (mapcat (fn [ws]
             (let [est-result (est-workspace-child est ws)
                   acc-path (map (fn [ep] (map #(:contents (lookup-hyp (:workspace ep) %))
                                          (:all (:accepted (:workspace ep)))))
                               (ep-path est-result))]
               (if (:best (:accrej ws))
                 ;; if something was last accepted, proceed
                 ;; recursively, but avoid repeating states
                 (if (not (seen-workspaces acc-path))
                   (do (set! seen-workspaces
                             (conj seen-workspaces acc-path))
                       (reason-exhaustive-recursively-cycle est-result time-now (inc depth)))
                   [])
                 ;; otherwise, that's the end of the line for this tree
                 [est-result])))
           ws-outcomes))))

(defn reason-exhaustive-recursively
  [est time-prev time-now sensors depth]
  (let [cycle (:cycle (cur-ep est))
        ws (:workspace (cur-ep est))
        ws-sensors (workspace-update-sensors ws time-prev time-now sensors cycle)
        est-sensors (est-workspace-child est ws-sensors)
        est-results (reason-exhaustive-recursively-cycle est-sensors time-now 0)]
    (if (empty? est-results) [est]
        (mapcat
         (fn [est-result]
           (comment
             (println "depth" depth
                      "count result"
                      (count (:hyp-ids (:workspace (cur-ep est-result))))
                      "count before" (count (:hyp-ids ws))))
           (if (and (:GetMoreHyps params)
                    (not= (count (:hyp-ids (:workspace (cur-ep est-result))))
                          (count (:hyp-ids ws))))
             (reason-exhaustive-recursively est-result time-prev time-now sensors
                                            (inc depth))
             [est-result]))
         est-results))))

(defn reason-exhaustive
  [est time-prev time-now sensors]
  (binding [seen-workspaces #{}]
    (let [est-results (reason-exhaustive-recursively est time-prev time-now sensors 0)
          est-final (first (sort compare-est est-results))]
      (comment
        (doseq [est est-results]
          (let [ws (:workspace (cur-ep est))]
            (println "result:")
            (println "\taccepted:" (map #(lookup-hyp ws %) (:all (:accepted ws))))
            (println "\tbest-each:" )
            (println "\tdoubts:" (filter identity (map #(calc-doubt %)
                                                (map :workspace (ep-path est)))))
            (println "\tnoexp:" (count (get-no-explainers ws)))
            (println "\tunexp:" (count (get-unexplained ws)))
            (println "\tdoubt:" (doubt-aggregate est)))))
      (comment
        (println "best:")
        (println "\taccepted:" (map #(lookup-hyp (:workspace (cur-ep est-final)) %)
                                  (:all (:accepted (:workspace (cur-ep est-final))))))
        (println "\tdoubt:" (doubt-aggregate est-final)))
      est-final)))

(defn reason
  [est time-prev time-now sensors & opts]
  (if (:Exhaustive params)
    (reason-exhaustive est time-prev time-now sensors)
    (loop [est est]
      (let [est-new (explain-and-advance est time-prev time-now sensors)
            meta? (and (not-any? #{:no-metareason} opts)
                       (metareasoning-activated? est-new))
            est-meta (if (not meta?) est-new
                         (metareason est-new time-prev time-now sensors))]
        ;; if something was accepted last, repeat
        (if (:best (:accrej (:workspace (cur-ep est-meta))))
          (recur est-meta) est-meta)))))

(defn meta-apply-and-evaluate
  [est est-new time-now sensors]
  (let [reason-est (reason est-new (:time (cur-ep est-new))
                           time-now sensors :no-metareason)]
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
                                             (/ (double (:MetaMinScore params)) 100.0))
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

(defn meta-lower-minscore
  [problem-cases est time-prev time-now sensors]
  (when (not= 0 (:MinScore params))
    (let [new-est (new-branch-ep est (cur-ep (goto-start-of-time est time-now)))]
      ;; drop min-score to 0
      (binding [params (assoc params :MinScore 0)]
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

(defn action-lower-minscore
  [time-now est]
  [(new-branch-ep est (cur-ep (goto-start-of-time est time-now)))
   (assoc params :MinScore 0)])

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
        eps (ep-path est)
        available-meta-hyps (set (str/split (:MetaHyps params) #","))]
    (concat
     (if (not (available-meta-hyps "order-dep")) []
         ;; order dependency among the observations; a no-expl-offered situation
         (if (not= 0 time-prev)
           (for [h (filter #(empty? (explainers cur-ws %)) problem-cases)]
             (let [t (:time (cur-ep (goto-cycle est (accepted-cycle cur-ws h))))
                   ep (cur-ep (goto-start-of-time est t))]
               (new-hyp "OrderDep" :order-dep :order-dep
                        1.0 false meta-hyp-conflicts? []
                        (format "Order dependency at %s" (str ep))
                        (format "Order dependency at %s" (str ep))
                        {:action (partial action-batch ep) :cycle (:cycle ep)})))
           ;; time-prev == 0, so this is a "static" case or we have not
           ;; done much reasoning yet
           []))
     ;; correct explainer(s) were rejected due to conflicts; need to
     ;; consider the various possibilities of rejected explainers and
     ;; no-explainers combinations
     (if (not (available-meta-hyps "rej-conflict")) []
         (let [expl-rejected-conflicts (filter (fn [h] (= :conflict (rejection-reason cur-ws h)))
                                          expl)
               acc-conflicting (set (mapcat
                                     (fn [e] (filter (fn [c] (accepted-before? cur-ws c e))
                                               (find-conflicts-all cur-ws e)))
                                     expl-rejected-conflicts))
               inner-hyps (set (map :id (mapcat :hyps acc-conflicting)))
               acc-conflicting-no-comps (filter #(not (inner-hyps (:id %)))
                                           acc-conflicting)
               ;; which ep-states rejected these expl and are
               ;; not states in which what was accepted was
               ;; essential?
               ep-rejs (filter (fn [ep] (and (some (set (map :contents acc-conflicting-no-comps))
                                          (map :contents (:acc (:accrej (:workspace ep)))))
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
             (println "acc-conflicting" acc-conflicting)
             (println "inner-hyps" inner-hyps)
             (println "acc-conflicting-no-comps" acc-conflicting-no-comps)
             (println "ep-rejs:" (map str ep-rejs) (map #(:nbest (:accrej (:workspace %))) ep-rejs))
             (println "bad-bests" bad-bests))
           (for [[delta cycle hyp] bad-bests]
             (new-hyp "Rejected" :rej-conflict :rej-conflict
                      0.5 false meta-hyp-conflicts? []
                      (format "%s rejected some explainers" hyp)
                      (format "%s rejected some explainers (cycle %d, delta %.2f"
                         (str hyp) cycle delta)
                      {:action (partial action-preemptively-reject hyp cycle)
                       :cycle cycle}))))
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
     ;; were some explainers omitted due to high min-score?
     (if (not (available-meta-hyps "rej-minscore")) []
         (let [expl-rejected-minscore (filter (fn [h] (= :minscore (rejection-reason cur-ws h)))
                                         expl)]
           (if (not-empty expl-rejected-minscore)
             [(new-hyp "TooHighMinScore" :rej-minscore :rej-minscore
                       0.25 false meta-hyp-conflicts? []
                       "Explainers rejected due to too-high min-score"
                       (format "These explainers were rejected due to too-high min-score: %s"
                          (str/join ", " (sort (map str expl-rejected-minscore))))
                       {:action (partial action-lower-minscore time-now)
                        :cycle (:cycle (cur-ep (goto-start-of-time est time-now)))})]
             []))))))

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
        meta-est-reasoned (binding [params (assoc params
                                             :MinScore 0
                                             :Threshold (:MetaThreshold params)
                                             :GetMoreHyps false)]
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
          {:est-old (goto-ep est-new-meta-est (:id (cur-ep est)))
           :est-new est-new-meta-est})))

(defn meta-abductive-recursive
  "Tries one abductive meta-hyp at a time until one works,
   recursively. Noise hyps are skipped; that can be handled
   below (resolve-by-ignoring)."
  [problem-cases est time-prev time-now sensors]
  (comment (println "problem-cases:" problem-cases))
  (comment (println "meta-hyps:" meta-hyps))
  (loop [est-attempt est
         hyps (sort-by :apriori (make-meta-hyps problem-cases est
                                                time-prev time-now sensors))
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
                             (sort-by :apriori (make-meta-hyps problem-cases-new
                                                               (:est-new result)
                                                               time-prev time-now sensors))
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
                (= "lower-minscore" m)
                meta-lower-minscore
                (= "reject-conflicting" m)
                meta-reject-conflicting
                (= "abd" m)
                meta-abductive
                (= "abd-recursive" m)
                meta-abductive-recursive
                (= "ignore" m)
                (constantly nil))
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
