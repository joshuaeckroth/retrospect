(ns retrospect.reason.abduction.reason
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace init-kb reset-workspace]])
  (:use [retrospect.reason.abduction.meta
         :only [metareasoning-activated? workspace-compare]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.logs
         :only [logs-tab update-logs]])
  (:use [retrospect.state]))

(def reason-abduction
  {:name "Abduction"
   :reason-fn (fn [truedata workspace time-prev time-now sensors]
                (let [ws (if (= "none" (:Oracle params)) workspace
                             (assoc workspace :oracle
                                    (partial (:true-hyp?-fn (:abduction @problem))
                                             truedata time-now)))]
                  (if sensors
                    (explain (add-sensor-hyps ws time-prev time-now sensors))
                    (explain ws))))
   :stats-fn (fn [truedata ors time-now] ((:stats-fn (:abduction @problem))
                                          truedata ors time-now))
   :metareasoning-activated?-fn metareasoning-activated?
   :workspace-compare-fn workspace-compare
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :default-params-fn (fn []
                        (merge {:Knowledge [80 [80]]
                                :BelievedKnowledge [80 [80]]
                                :Threshold [0 [0]]
                                :DoubtThreshold [1000 [1000]]
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
