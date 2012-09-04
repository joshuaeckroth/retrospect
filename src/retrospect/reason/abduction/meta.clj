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
  (:use [retrospect.state]))

(defn reason
  "We assume some other code has created the child ep-state with the right time."
  [truedata est time-prev time-now sensors]
  (let [workspace (:workspace (cur-ep est))
        ws (if (= "none" (:Oracle params))
             workspace
             (assoc workspace :oracle
                    (partial (:true-hyp?-fn (:abduction @problem))
                             truedata time-now)))
        ws-sensors (if sensors
                     (update-hypotheses
                      (add-sensor-hyps ws time-prev time-now sensors) time-now)
                     (update-hypotheses ws time-now))
        est-ws (update-est est (assoc (cur-ep est) :workspace ws-sensors :time time-now))]
    (explain est-ws)))

(defn find-problem-cases
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (set (map #(lookup-hyp workspace %) (get-no-explainers workspace)))))

(defn meta-apply-and-evaluate
  [truedata est est-new time-prev time-now sensors]
  (let [reason-est (reason (when (:Oracle params) truedata) est-new
                           time-prev time-now sensors)]
    {:est-old (goto-ep reason-est (:id (cur-ep est)))
     :est-new reason-est}))

(comment
  (defn apply-resolutions
    [accepted est time-prev time-now sensors]
    (if (empty? accepted)
      {:est est :considered? false :accepted-branch? false}
      (let [new-est (new-branch-ep est (cur-ep est))
            new-ep (cur-ep new-est)
            ws-old (:workspace new-ep)
            ;; apply all the actions specified by the accepted meta-hyps;
            ;; always revert workspace first
            ws-new (reduce (fn [ws h] ((:action h) ws))
                      (revert-workspace ws-old) accepted)
            ws-expl (reason (when (:Oracle params) truedata) ws-new
                            (if (some #{:anomaly} (map :type accepted)) 0 time-prev)
                            time-now sensors)
            new-expl-est (update-est new-est (assoc new-ep :workspace ws-expl))]
        (if (workspace-better? ws-expl ws-old)
          {:est new-expl-est
           :considered? true
           :accepted-branch? true}
          {:est (goto-ep new-expl-est (:id (cur-ep est)))
           :considered? true
           :accepted-branch? false})))))

(comment
  (defn belief-revision
    [noexp-hyp depth workspace]
    ;; go back "before" this depth in order to repair
    (revert-workspace workspace (dec depth))))

(comment
  (defn transitive-explanation
    [noexp-hyp workspace]
    ;; how to do this? this will explain unexplained-impasses, not
    ;; noexp-impasses
    workspace))

(comment
  (defn make-noexp-hyps
    [noexp]
    (for [ne noexp]
      (new-hyp "NoExp" :noexp :noexp 1.0 true nil []
               (format "NoExp: %s" ne)
               (format "%s has no explainer but needs an explanation." ne)
               {:hyp ne}))))

(comment
  )

(comment
  )

(comment
  )

(comment
  (defn metareason
    [truedata est time-prev time-now sensors]
    (let [workspace (:workspace (cur-ep est))
          ws-depth (workspace-depth workspace)
          noexp (map (partial lookup-hyp workspace) (get-no-explainers workspace))
          noexp-hyps (make-noexp-hyps noexp)
          meta-hyps (make-meta-hyps workspace noexp-hyps ws-depth time-now)
          meta-ws (reduce add (reduce add-observation (init-workspace) noexp-hyps) meta-hyps)
          meta-ws-explained (explain meta-ws)
          ep-meta (assoc (cur-ep est) :meta-workspace meta-ws-explained)
          est-meta (update-est est ep-meta)
          accepted (map (partial lookup-hyp meta-ws-explained)
                      (concat (:anomaly (:accepted meta-ws-explained))
                              (:noise (:accepted meta-ws-explained))
                              (:learn (:accepted meta-ws-explained))))]
      (apply-resolutions accepted est-meta time-prev time-now sensors))))

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
  [n problem-caes truedata est _ time-now sensors]
  (let [time (if n (- time-now n) 0)
        new-est (new-branch-ep est (cur-ep (goto-start-of-time est time)))]
    (meta-apply-and-evaluate truedata est new-est (:time (cur-ep new-est))
                             time-now sensors)))

(defn meta-batch-weakest
  [problem-cases truedata est _ time-now sensors]
  (let [doubts-cycles (reverse (sort-by first
                                        (map (fn [ep]
                                             [(calc-doubt (:workspace ep)) (:cycle ep)])
                                           (ep-path est))))
        highest-doubt (ffirst doubts-cycles)
        prior-cycle (dec (second (first (sort-by second (filter #(= highest-doubt (first %))
                                                           doubts-cycles)))))
        new-est (new-branch-ep est (cur-ep (goto-cycle est prior-cycle)))]
    (meta-apply-and-evaluate truedata est new-est (:time (cur-ep new-est))
                             time-now sensors)))

(defn meta-lower-minapriori
  [problem-cases truedata est time-prev time-now sensors]
  (when (not= 0 (:MinApriori params))
    (let [new-est (new-branch-ep est (cur-ep (goto-start-of-time est time-prev)))]
      ;; drop min-apriori to 0
      (binding [params (assoc params :MinApriori 0)]
        ;; give sensors value as nil to prevent resensing
        (meta-apply-and-evaluate truedata est new-est time-prev time-now sensors)))))

(defn preemptively-reject
  [est hyp]
  (let [ep (cur-ep est)
        ws-rejected (reject-many (:workspace ep) [hyp] :preemptive)
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
  [est problem-cases]
  (let [expl (set (explainers (:workspace (cur-ep est)) problem-cases))
        ;; which ep-states rejected these expl?
        ep-rejs (filter (fn [ep] (some (set (map :contents expl))
                              (map :contents (:rej (:accrej (:workspace ep))))))
                   (ep-path est))
        bad-bests (sort-by first
                           ;; don't consider a next-best that still
                           ;; conflicts with what can explain the
                           ;; noexp; note, the next-best may conflict
                           ;; but a different alternative may not --
                           ;; we are not finding that other
                           ;; alternative
                           (filter (fn [[_ _ h]]
                                ;; also ignore any non-bests (no-acceptance) cases
                                (and h (not-any? (fn [e] (conflicts? h e)) expl)))
                              (set (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                               (:cycle ep)
                                               (get-in ep [:workspace :accrej :best])])
                                      ep-rejs))))
        smallest-delta (ffirst bad-bests)
        [_ cycle best] (first (sort-by second (filter #(= smallest-delta (first %)) bad-bests)))]
    ;; TODO: consider the delta; if it's too large, perhaps the noexp is just noise
    (when cycle
      (let [new-est (new-branch-ep est (cur-ep (goto-cycle est (dec cycle))))]
        (preemptively-reject new-est best)))))

(defn meta-reject-conflicting
  [problem-cases truedata est time-prev time-now sensors]
  (let [est-rej (find-rejected-explainers est (first problem-cases))]
    (when est-rej
      (meta-apply-and-evaluate truedata est est-rej
                               (:time (cur-ep est-rej)) time-now sensors))))

(defn action-batch
  [ep est]
  [(new-branch-ep est ep)
   params])

(defn action-flip-choice
  [ep est]
  [est params])

(defn action-lower-minapriori
  [time-prev est]
  [(new-branch-ep est (cur-ep (goto-start-of-time est time-prev)))
   (assoc params :MinApriori 0)])

(defn action-ignore
  [hyp est]
  (let [ws-old (:workspace (cur-ep est))
        new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-ignored (reject-many (:workspace new-ep) [hyp] :ignoring)]
    [(update-est new-est (assoc new-ep :workspace ws-ignored))
     params]))

(defn action-preemptively-reject
  [hyp cycle est]
  (let [new-est (new-branch-ep est (cur-ep (goto-cycle est (dec cycle))))
        ep (cur-ep new-est)
        ws-rejected (reject-many (:workspace ep) [hyp] :preemptive)
        ep-rejected (assoc ep :workspace ws-rejected)]
    [(update-est est ep-rejected)
     params]))

(defn meta-hyp-conflicts?
  [hyp1 hyp2]
  (and (not= hyp1 hyp2)
       (or (not-empty (:explains hyp1))
           (not-empty (:explains hyp2)))
       (= (:explains hyp1) (:explains hyp2))))

(defn make-meta-hyps
  "Create explanations, and associated actions, for problem-cases."
  [problem-cases truedata est time-prev time-now sensors]
  (let [cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) problem-cases))
        eps (ep-path est)]
    (concat
     ;; order dependency among the observations
     (if (not= 0 time-prev)
       (for [ep (map (fn [t] (cur-ep (goto-start-of-time est t))) (range 1 time-now))]
         (new-hyp "OrderDep" :order-dep :weakest
                  1.0 false meta-hyp-conflicts? []
                  (format "Order dependency at %s" (str ep))
                  (format "Order dependency at %s" (str ep))
                  {:action (partial action-batch ep)}))
       ;; time-prev == 0, so this is a "static" case or we have not
       ;; done much reasoning yet
       [])
     ;; wrong choice at a certain decision (batch + flip)
     (for [ep (filter (comp :nbest :accrej :workspace) eps)]
       (new-hyp "WrongChoice" :wrong-choice :wrong-choice
                1.0 false meta-hyp-conflicts? []
                (format "Wrong choice at %s" (str ep))
                (format "%s was accepted at %s but %s should be instead."
                   (str (:best (:accrej (:workspace ep)))) (str ep)
                   (str (:nbest (:accrej (:workspace ep)))))
                {:action (partial action-flip-choice ep)}))
     ;; were some explainers omitted due to high min-apriori?
     (let [expl-rejected-minapriori (filter (fn [h] (= :minapriori (rejection-reason cur-ws h)))
                                       expl)]
       (if (not-empty expl-rejected-minapriori)
         [(new-hyp "TooHighMinApriori" :rejected :minapriori
                   1.0 false meta-hyp-conflicts? []
                   "Explainers rejected due to too-high min-apriori"
                   (format "These explainers were rejected due to too-high min-apriori: %s"
                      (str/join ", " (sort (map str expl-rejected-minapriori))))
                   {:action (partial action-lower-minapriori time-prev)})]
         []))
     ;; noise hyps
     (for [noexp (filter #(= :observation (:type %)) problem-cases)]
       (new-hyp "Noise" :noise :insertion-noise
                1.0 false meta-hyp-conflicts? []
                (format "%s is insertion noise" noexp) (format "%s is insertion noise" noexp)
                {:action (partial action-ignore noexp)}))
     ;; correct explainer(s) were rejected due to conflicts; need to
     ;; consider the various possibilities of rejected explainers and
     ;; no-explainers combinations
     (let [expl-rejected-conflicts (filter (fn [h] (= :conflict (rejection-reason cur-ws h)))
                                      expl)
           ;; which ep-states rejected these expl?
           ep-rejs (filter (fn [ep] (some (set (map :contents expl-rejected-conflicts))
                                 (map :contents (:rej (:accrej (:workspace ep))))))
                      (ep-path est))
           bad-bests (sort-by first
                              ;; don't consider a next-best that still
                              ;; conflicts with what can explain the
                              ;; noexp; note, the next-best may conflict
                              ;; but a different alternative may not --
                              ;; we are not finding that other
                              ;; alternative
                              (filter (fn [[_ _ h]]
                                   ;; also ignore any non-bests (no-acceptance) cases
                                   (and h (not-any? (fn [e] (conflicts? h e))
                                                    expl-rejected-conflicts)))
                                 (set (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                                  (:cycle ep)
                                                  (get-in ep [:workspace :accrej :best])])
                                         ep-rejs))))]
       (for [[delta cycle hyp] bad-bests]
         (new-hyp "Rejected" :rejected :conflict
                  (- 1.0 delta) false meta-hyp-conflicts? []
                  (format "%s rejected some explainers")
                  (format "%s rejected some explainers (cycle %d, delta %.2f"
                     (str hyp) cycle delta)
                  {:action (partial action-preemptively-reject hyp cycle)}))))))

(defn score-meta-hyps
  [problem-cases meta-hyps truedata est time-prev time-now sensors]
  (loop [est est
         hyps meta-hyps
         new-hyps []]
    (if (empty? hyps) [est new-hyps]
        (let [hyp (first hyps)
              [est-new params-new] ((:action hyp) est)
              result (binding [params params-new]
                       (meta-apply-and-evaluate truedata est est-new
                                                (:time (cur-ep est-new)) time-now sensors))
              doubts-new (map #(calc-doubt %)
                            (filter (comp :best :accrej)
                               (map :workspace (ep-path (:est-new result)))))
              doubt-new (if (empty? doubts-new) 0.0 (/ (reduce + doubts-new) (count doubts-new)))
              problem-cases-new (find-problem-cases (:est-new result))
              resolved-cases (filter (fn [pc] (not ((set (map :contents problem-cases-new))
                                              (:contents pc))))
                                problem-cases)]
          (recur (:est-old result) (rest hyps)
                 (conj new-hyps
                       (assoc hyp :explains (map :contents resolved-cases)
                              :apriori (- 1.0 doubt-new))))))))

(defn meta-abductive
  [problem-cases truedata est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps problem-cases truedata est time-prev time-now sensors)
        [est-new meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps
                                                    truedata est time-prev time-now sensors)
        meta-est (new-child-ep (init-est (init-workspace)))
        meta-ws (reduce add (reduce add-observation (:workspace (cur-ep meta-est)) problem-cases)
                   meta-hyps-scored)
        meta-est-explained (explain (update-est meta-est (assoc (cur-ep meta-est)
                                                           :workspace meta-ws)))
        meta-ws-explained (:workspace (cur-ep meta-est-explained))
        ;; take out the "observations"
        meta-accepted (filter (fn [hyp] (not ((set (map :contents problem-cases)) (:contents hyp))))
                         (map #(lookup-hyp meta-ws-explained %)
                            (:all (:accepted meta-ws-explained))))]
    (println "problem cases:" (str/join ", " (map str problem-cases)))
    (println (for [h meta-hyps-scored]
               (format "%s explains %s\n\n" (str h)
                  (str/join ", " (map str (filter (fn [pc] ((set (:explains h)) (:contents pc)))
                                           problem-cases))))))
    (println "accepted" meta-accepted))
  (comment
    (if (not= 0 (:SensorInsertionNoise params))
      {:est-old est
       :est-new est}
      (let [batch1 (meta-batch 1 truedata est time-prev time-now sensors)]
        (if (empty? (problem-cases (:est-new batch1))) batch1
            (let [batch1-lowermin (meta-batch-lower-min-apriori
                                   1 truedata (:est-old batch1)
                                   time-prev time-now sensors)]
              (if (empty? (problem-cases (:est-new batch1-lowermin))) batch1-lowermin
                  (let [batch-weakest (meta-batch-weakest
                                       nil truedata (:est-old batch1-lowermin)
                                       time-prev time-now sensors)]
                    (if (empty? (problem-cases (:est-new batch-weakest))) batch-weakest
                        (meta-batch nil truedata (:est-old batch-weakest)
                                    time-prev time-now sensors))))))))))

(defn resolve-by-ignoring
  [problem-cases truedata est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-old (:workspace (cur-ep est))
        ws-ignored (reject-many (:workspace new-ep) problem-cases :ignoring)
        new-est-ignored (update-est new-est (assoc new-ep :workspace ws-ignored))]
    (reason (when (:Oracle params) truedata) new-est-ignored time-prev time-now sensors)))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [truedata est time-prev time-now sensors]
  (let [problem-cases (find-problem-cases est)]
    (if (or (empty? problem-cases)
            (= "none" (:Metareasoning params)))
      est
      (let [m (:Metareasoning params)
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
                    (= "batch-weakest" m)
                    meta-batch-weakest
                    (= "lower-minapriori" m)
                    meta-lower-minapriori
                    (= "reject-conflicting" m)
                    meta-reject-conflicting
                    (= "abd" m)
                    meta-abductive
                    (= "ignore" m)
                    (constantly {:est-old est :est-new est}))
            result (f problem-cases truedata est time-prev time-now sensors)
            problem-cases-new (when result (find-problem-cases (:est-new result)))]
        (cond (nil? result)
              (resolve-by-ignoring problem-cases truedata est
                                   time-prev time-now sensors)
              (empty? problem-cases-new)
              (:est-new result)
              (< (count problem-cases-new) (count problem-cases))
              (resolve-by-ignoring problem-cases-new truedata (:est-new result)
                                   time-prev time-now sensors)
              :else
              (resolve-by-ignoring problem-cases truedata (:est-old result)
                                   time-prev time-now sensors))))))
