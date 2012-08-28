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

(defn belief-revision
  [noexp-hyp depth workspace]
  ;; go back "before" this depth in order to repair
  (revert-workspace workspace (dec depth)))

(defn transitive-explanation
  [noexp-hyp workspace]
  ;; how to do this? this will explain unexplained-impasses, not
  ;; noexp-impasses
  workspace)

(defn ignore-hyp
  [noexp-hyp workspace]
  (add-kb workspace
          [(new-hyp "Ignore" :kb :ignore 1.0 false
                    (:conflicts?-fn (:hyp noexp-hyp))
                    []
                    (format "Ignore %s" (:hyp noexp-hyp))
                    (format "Ignore %s" (:hyp noexp-hyp))
                    (:data (:hyp noexp-hyp)))]))

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
  (concat
   (mapcat (fn [ne] (for [i (range (dec ws-depth) 0 -1)]
                     (new-hyp "Anomaly" :anomaly :anomaly
                              (calc-doubt (revert-workspace ws-original i))
                              false conflicts? [(:contents ne)]
                              (format "%s is an anomaly from depth %d" ne i)
                              (format "%s is an anomaly from depth %d" ne i)
                              {:action (partial belief-revision ne i) :noexp-hyp ne
                               :depth i})))
           noexp-hyps)
   (mapcat (fn [ne]
             [(new-hyp "Noise" :noise :insertion-noise
                       (+ 0.5 (* 0.5 (/ (double (:SensorInsertionNoise params)) 100.0)))
                       false conflicts? [(:contents ne)]
                       (format "%s is insertion noise" ne) (format "%s is insertion noise" ne)
                       {:action (partial ignore-hyp ne) :noexp-hyp ne})])
           (filter #(= :observation (:type (:hyp %))) noexp-hyps))))

(comment
  ;; noise hyps
  (if (or (= "abd-no-noise" (:Metareasoning params))
          (and (= 0 (:SensorInsertionNoise params))
               (= 0 (:SensorDistortionNoise params))
               (= 0 (:SensorDeletionNoise params))))
    []
    ))

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

(defn metareason
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
            ;; apply noise hyps (give up)
            (apply-resolutions (filter #(= :noise (:type %)) meta-hyps)
                               est-meta time-prev time-now sensors)
            ;; if we haven't attempted this depth yet
            (not (attempted-depths (:depth (first hyps))))
            (let [result (apply-resolutions [(first hyps)]
                                            est-meta time-prev time-now sensors)]
              (if (:accepted-branch? result) result
                  (recur (:est result) (rest hyps)
                         (conj attempted-depths (:depth (first hyps))) (inc attempts))))
            ;; else, already have attempted this depth; move on to next hyp
            :else
            (recur est-meta (rest hyps) attempted-depths attempts)))))

