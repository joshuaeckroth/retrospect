(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est
          update-est goto-start-of-time print-est goto-ep ep-path goto-cycle]])
  (:use [retrospect.reason.abduction.workspace :only
         [get-no-explainers get-unexplained new-hyp init-workspace calc-doubt
          explain add-kb add-observation add lookup-hyp explainers
          update-kb explain update-hypotheses add-sensor-hyps conflicts?
          reject-many]])
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
        est-ws (update-est est (assoc (cur-ep est) :workspace ws-sensors))]
    (explain est-ws)))

(defn problem-cases
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (set (get-no-explainers workspace))))

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [est]
  (not-empty (problem-cases est)))

(defn meta-apply-and-evaluate
  [truedata est new-est time-prev time-now sensors]
  (let [reason-est (reason (when (:Oracle params) truedata) new-est
                           time-prev time-now sensors)]
    {:est-old (goto-ep reason-est (:id (cur-ep est)))
     :est-new (update-est reason-est (assoc (cur-ep reason-est) :time time-now))}))

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
  (defn conflicts?
    [hyp1 hyp2]
    (= (:explains hyp1) (:explains hyp2))))

(comment
  (defn make-meta-hyps
    "Create explanations, and associated actions, for noexp."
    [ws-original noexp-hyps ws-depth time-now]
    ;; anomaly hyps
    (mapcat (fn [ne] (for [i (range (dec ws-depth) 0 -1)]
                      (new-hyp "Anomaly" :anomaly :anomaly
                               (calc-doubt (revert-workspace ws-original i))
                               false conflicts? [(:contents ne)]
                               (format "%s is an anomaly from depth %d" ne i)
                               (format "%s is an anomaly from depth %d" ne i)
                               {:action (partial belief-revision ne i) :noexp-hyp ne
                                :depth i})))
            noexp-hyps)))

(comment
  (mapcat (fn [ne]
            [(new-hyp "Noise" :noise :insertion-noise
                      (+ 0.5 (* 0.5 (/ (double (:SensorInsertionNoise params)) 100.0)))
                      false conflicts? [(:contents ne)]
                      (format "%s is insertion noise" ne) (format "%s is insertion noise" ne)
                      {:action (partial ignore-hyp (:hyp ne)) :noexp-hyp ne})])
          (filter #(= :observation (:type (:hyp %))) noexp-hyps)))

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
  [n truedata est _ time-now sensors]
  (let [time (if n (- time-now n) 0)
        new-est (new-branch-ep est (cur-ep (goto-start-of-time est time)))]
    (meta-apply-and-evaluate truedata est new-est (:time (cur-ep new-est))
                             time-now sensors)))

(defn meta-batch-weakest
  [truedata est _ time-now sensors]
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
  [truedata est time-prev time-now sensors]
  (when (not= 0 (:MinApriori params))
    (let [new-est (new-branch-ep est (cur-ep est))]
      ;; drop min-apriori to 0
      (binding [params (assoc params :MinApriori 0)]
        ;; give sensors value as nil to prevent resensing
        (meta-apply-and-evaluate truedata est new-est time-prev time-now nil)))))

(defn preemptively-reject
  [est hyp]
  (let [ep (cur-ep est)
        ws-rejected (reject-many (:workspace ep) [hyp])
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
  [est noexp]
  (let [expl (set (explainers (:workspace (cur-ep est))
                              (lookup-hyp (:workspace (cur-ep est)) noexp)))
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
                           (filter (fn [h] (not-any? (fn [e] (conflicts? h e)) expl))
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
  [truedata est time-prev time-now sensors]
  (let [problem-cases (problem-cases est)
        est-rej (when (not-empty problem-cases)
                  (find-rejected-explainers est (first problem-cases)))]
    (when est-rej
      (meta-apply-and-evaluate truedata est est-rej
                               (:time (cur-ep est-rej)) time-now sensors))))

(defn meta-abductive
  [truedata est time-prev time-now sensors]
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

(defn ignore-hyp
  [workspace hypid]
  (let [hyp (lookup-hyp workspace hypid)]
    (add workspace (new-hyp "Ignore" :ignore :ignore 1.0 false (:conflicts?-fn hyp)
                            [(:contents hyp)]
                            (format "Ignore %s" hyp) (format "Ignore %s" hyp)
                            (:data hyp)))))

(defn resolve-by-ignoring
  [problem-cases truedata est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-old (:workspace new-ep)
        ws-ignored (reduce ignore-hyp ws-old problem-cases)
        new-est-ignored (update-est new-est (assoc new-ep :workspace ws-ignored))]
    (reason (when (:Oracle params) truedata) new-est-ignored time-prev time-now sensors)))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [truedata est time-prev time-now sensors]
  (if (or (not (metareasoning-activated? est))
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
                  meta-abductive)
          result (f truedata est time-prev time-now sensors)
          problem-cases-old (when result (problem-cases (:est-old result)))
          problem-cases-new (when result (problem-cases (:est-new result)))]
      (cond (nil? result)
            (resolve-by-ignoring problem-cases-old truedata est
                                 time-prev time-now sensors)
            (empty? problem-cases-new)
            (:est-new result)
            (< (count problem-cases-new) (count problem-cases-old))
            (resolve-by-ignoring problem-cases-new truedata (:est-new result)
                                 time-prev time-now sensors)
            :else
            (resolve-by-ignoring problem-cases-old truedata (:est-old result)
                                 time-prev time-now sensors)))))
