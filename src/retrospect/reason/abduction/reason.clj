(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace
                update-hypotheses init-kb update-kb reset-workspace accepted?
                calc-doubt calc-coverage extract-training lookup-hyp
                inject-true-hyps find-no-explainers]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [run-scorer get-words]])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate :only [evaluate-helper]])
  (:use [retrospect.reason.abduction.meta
         :only [metareasoning-activated? workspace-compare]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp update-training
                group-hyps-by-true-false]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.logs
         :only [logs-tab update-logs]])
  (:use [retrospect.state]))

(defn find-false-accepted
  [workspace true-false-types]
  (filter #(accepted? workspace %) (get-in true-false-types [:all false])))

(defn reason-train
  [truedata workspace time-prev time-now sensors]
  (let [ws-orig (if (= "none" (:Oracle params)) workspace
                    (assoc workspace :oracle
                           (partial (:true-hyp?-fn (:abduction @problem))
                                    truedata time-now)))]
    (loop [ws ws-orig
           cycle (:TrainingCycles params)]
      (let [ws-result
            (if sensors
              (explain (update-hypotheses
                        (add-sensor-hyps ws time-prev time-now sensors)))
              (explain (update-hypotheses ws)))
            true-false-types (group-hyps-by-true-false
                              (vals (:hyp-ids ws-result))
                              :type truedata ws-result
                              time-now (:true-hyp?-fn (:abduction @problem)))
            false-accepted (find-false-accepted ws-result true-false-types)
            no-explainers (find-no-explainers ws-result)
            unexplained (set/difference (set (:unexplained (:log ws-result)))
                                        (set no-explainers))]
        (when (:TrainingStats params)
          ((:training-stats-fn (:abduction @problem))
           ws-result false-accepted unexplained truedata time-now cycle))
        (cond (not training?)
              (update-kb ws-result)
              (and (= 0 (count false-accepted))
                   (= 0 (count unexplained)))
              (update-kb ws-result)
              (= 0 cycle) ;; done training; ensure only correct stuff is accepted
              (update-kb (inject-true-hyps ws-result true-false-types))
              :else
              (let [ws-scored (update-training
                               ws-result true-false-types
                               unexplained)]
                (recur (assoc ws :scores (:scores ws-scored)
                              :score-adjustments (:score-adjustments ws-scored))
                       (dec cycle))))))))

(def reason-abduction
  {:name "Abduction"
   :reason-fn reason-train
   :stats-fn (fn [truedata ors time-now] ((:stats-fn (:abduction @problem))
                                          truedata ors time-now))
   :metareasoning-activated?-fn metareasoning-activated?
   :workspace-compare-fn workspace-compare
   :extract-training-fn extract-training
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :calc-doubt-fn calc-doubt
   :calc-coverage-fn calc-coverage
   :default-params-fn (fn []
                        (merge {:Threshold [0 [0]]
                                :ConfThreshold [0 [0]]
                                :UseScores [true [true]]
                                :ContrastPreference ["delta" ["delta" "arbitrary"]]
                                :ApplyBoosting [true [true false]]
                                :HypPreference ["abd" ["abd" "arbitrary"]]
                                :TransitiveExplanation [false [true false]]
                                :TrainingCycles [10 [10]]
                                :TrainingAdjustment [0.1 [0.1]]
                                :ConfAdjustment ["none" ["min" "max" "avg" "none" "norm"]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :reset-workspace-fn reset-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
