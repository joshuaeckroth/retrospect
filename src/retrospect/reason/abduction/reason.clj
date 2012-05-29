(ns retrospect.reason.abduction.reason
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace
                update-hypotheses init-kb update-kb reset-workspace
                calc-doubt calc-coverage extract-training]])
  (:use [retrospect.reason.abduction.meta
         :only [metareasoning-activated? workspace-compare]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp update-training
                count-false-accepted]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.logs
         :only [logs-tab update-logs]])
  (:use [retrospect.state]))

(defn reason-train
  [truedata workspace time-prev time-now sensors]
  (let [ws-orig (if (= "none" (:Oracle params)) workspace
                    (assoc workspace :oracle
                           (partial (:true-hyp?-fn (:abduction @problem))
                                    truedata time-now)))]
    (loop [ws ws-orig
           temp 1.0] ;; "temperature"
      (let [ws-result
            (if sensors
              (explain (update-hypotheses
                        (add-sensor-hyps ws time-prev time-now sensors)))
              (explain (update-hypotheses ws)))]
        (cond (or (not training?)
                  (>= 0.0 temp)
                  (= 0 (count-false-accepted ws-result truedata time-now)))
              (update-kb ws-result)
              :else
              (recur (assoc ws :scores
                            (:scores (update-training ws-result truedata time-now temp)))
                     (- temp 0.2)))))))

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
                                :ConfAdjustment ["max" ["min" "max" "avg" "none" "norm"]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :reset-workspace-fn reset-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
