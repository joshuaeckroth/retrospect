(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est ep-state-depth
          update-est nth-previous-ep print-est goto-ep
          get-init-workspace]])
  (:use [retrospect.reason.abduction.workspace :only
         [get-no-explainers get-unexplained new-hyp init-workspace calc-doubt
          explain add-kb add-observation add lookup-hyp
          reset-workspace revert-workspace workspace-depth
          update-kb explain update-hypotheses add-sensor-hyps]])
  (:use [retrospect.state]))

(defn reason
  [truedata workspace time-prev time-now sensors]
  (let [ws (assoc (if (= "none" (:Oracle params))
                    workspace
                    (assoc workspace :oracle
                           (partial (:true-hyp?-fn (:abduction @problem))
                                    truedata time-now)))
             :prior-workspace workspace
             :depth (inc (:depth workspace)))]
    (if sensors
      (update-kb (explain (update-hypotheses
                           (add-sensor-hyps ws time-prev time-now sensors)
                           time-now)))
      (update-kb (explain (update-hypotheses ws time-now))))))

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (not-empty (get-no-explainers workspace))))

(defn workspace-better?
  [ws-new ws-old]
  ;; we wanted to fix "no explainers"; so, did we?
  (empty? (get-no-explainers ws-new)))

(defn ignore-hyp
  [noexp workspace]
  (add-kb workspace
          [(new-hyp "Ignore" :kb :ignore 1.0 false
                    (:conflicts?-fn noexp)
                    []
                    (format "Ignore %s" noexp)
                    (format "Ignore %s" noexp)
                    (:data noexp))]))

(defn meta-apply-and-evaluate
  [truedata est new-est time-prev time-now sensors]
  (let [new-ep (cur-ep new-est)
        ws-old (:workspace new-ep)
        ws-new (reason
                (when (:Oracle params) truedata)
                (if (or (nil? sensors) (not (:ResetEachStep params))) ws-old
                    (reset-workspace ws-old))
                time-prev time-now sensors)
        new-expl-est (update-est new-est (assoc new-ep :workspace ws-new))]
    (if (workspace-better? ws-new ws-old)
      {:est new-expl-est
       :considered? true
       :accepted-branch? true}
      ;; trigger for metareasoning not resolved; ignore the 'noexp' hyps
      {:est (goto-ep new-expl-est (:id (cur-ep est)))
       :considered? true
       :accepted-branch? false})))

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
         :accepted-branch? false}))))

(defn belief-revision
  [noexp-hyp depth workspace]
  ;; go back "before" this depth in order to repair
  (revert-workspace workspace (dec depth)))

(defn transitive-explanation
  [noexp-hyp workspace]
  ;; how to do this? this will explain unexplained-impasses, not
  ;; noexp-impasses
  workspace)

(defn make-noexp-hyps
  [noexp]
  (for [ne noexp]
    (new-hyp "NoExp" :noexp :noexp 1.0 true nil []
             (format "NoExp: %s" ne)
             (format "%s has no explainer but needs an explanation." ne)
             {:hyp ne})))

(defn conflicts?
  [hyp1 hyp2]
  (= (:explains hyp1) (:explains hyp2)))

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
          noexp-hyps))

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
  (let [branch-root? (or (nil? n) (>= n (ep-state-depth est)))
        branch-ep (nth-previous-ep est n)
        new-est (new-branch-ep est branch-ep)
        new-est-time (update-est new-est
                                 (assoc (cur-ep new-est) :time time-now
                                        :workspace (if branch-root?
                                                     (get-init-workspace est)
                                                     (:workspace (cur-ep new-est)))))]
    (meta-apply-and-evaluate truedata est new-est-time
                             (if branch-root? 0 (:time branch-ep))
                             time-now sensors)))

(defn meta-batch-weakest
  [max-n truedata est _ time-now sensors]
  (let [workspace (:workspace (cur-ep est))
        ws-depth (workspace-depth workspace)
        depth-doubts (for [i (range (dec ws-depth) -1 -1)]
                       [i (calc-doubt (revert-workspace workspace i))])
        n (max max-n (- ws-depth (ffirst (reverse (sort-by second depth-doubts)))))
        branch-root? (>= n (ep-state-depth est))
        branch-ep (nth-previous-ep est n)
        new-est (new-branch-ep est branch-ep)
        new-est-time (update-est new-est
                                 (assoc (cur-ep new-est) :time time-now
                                        :workspace (if branch-root?
                                                     (get-init-workspace est)
                                                     (:workspace (cur-ep new-est)))))]
    (meta-apply-and-evaluate truedata est new-est-time
                             (if branch-root? 0 (:time branch-ep))
                             time-now sensors)))

(defn meta-lower-threshold
  [truedata est time-prev time-now sensors]
  (if (= 0 (:Threshold params))
    {:est est :considered? false :accepted-branch? false}
    (let [new-est (new-branch-ep est (cur-ep est))]
      ;; drop threshold to 0
      (binding [params (assoc params :Threshold 0)]
        ;; give sensors value as nil to prevent resensing
        (meta-apply-and-evaluate truedata est new-est time-prev time-now nil)))))

(defn meta-batch-lower-threshold
  [n truedata est time-prev time-now sensors]
  (let [batch-result (meta-batch n truedata est time-prev time-now sensors)]
    (if (or (= 0 (:Threshold params)) (:accepted-branch? batch-result))
      batch-result
      ;; how much to lower threshold?
      (binding [params (assoc params :Threshold 0)]
        (meta-batch n truedata est time-prev time-now sensors)))))

(defn meta-batch-lower-min-apriori
  [n truedata est time-prev time-now sensors]
  (let [batch-result (meta-batch n truedata est time-prev time-now sensors)]
    (if (or (= 0 (:MinApriori params)) (:accepted-branch? batch-result))
      batch-result
      ;; how much to lower threshold?
      (binding [params (assoc params :MinApriori 0)]
        (meta-batch n truedata est time-prev time-now sensors)))))

(defn meta-abductive
  [truedata est time-prev time-now sensors]
  (let [batch1 (meta-batch 1 truedata est time-prev time-now sensors)]
    (if (:accepted-branch? batch1) batch1
        (let [batch-weakest (meta-batch-weakest truedata (:est batch1)
                                                time-prev time-now sensors)]
          (if (:accepted-branch? batch-weakest) batch-weakest
              (meta-batch nil truedata (:est batch-weakest)
                          time-prev time-now sensors))))))

(defn force-resolve-trigger
  [truedata est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-old (:workspace new-ep)
        noexp (map #(lookup-hyp ws-old %) (get-no-explainers ws-old))
        ws-new (reduce (fn [ws h] (ignore-hyp h ws))
                  (revert-workspace ws-old) noexp)
        ws-expl (reason (when (:Oracle params) truedata) ws-new
                        time-prev time-now sensors)]
    (update-est new-est (assoc new-ep :workspace ws-expl))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [truedata est time-prev time-now sensors]
  (if (or (not (metareasoning-activated? est))
          (= "none" (:Metareasoning params)))
    {:est est :considered? false :accepted-branch? false}
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
                  (= "batch10" m)
                  (partial meta-batch 10)
                  (= "batchweakest10" m)
                  (partial meta-batch-weakest 10)
                  (= "lowerthresh" m)
                  meta-lower-threshold
                  (= "batch1-lowerthresh" m)
                  (partial meta-batch-lower-threshold 1)
                  (= "batch1-lowermin" m)
                  (partial meta-batch-lower-min-apriori 1)
                  ;; did not recognize the metareasoning strategy;
                  ;; hand-off to the reasoning engine
                  :else
                  meta-abductive)
          result (f truedata est time-prev time-now sensors)]
      (if (:accepted-branch? result) result
          (assoc result :est
                 (force-resolve-trigger truedata (:est result)
                                        time-prev time-now sensors))))))
