(ns retrospect.reason.abduction.reason
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace init-kb]])
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
                (let [ws (if (not (:Oracle params)) workspace
                             (assoc workspace :oracle
                                    (partial (:true-hyp?-fn (:abduction @problem))
                                             truedata time-now)))]
                  (explain (add-sensor-hyps ws time-prev time-now sensors))))
   :stats-fn (fn [truedata ors time-now] ((:stats-fn (:abduction @problem)) truedata ors time-now))
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
                                :LearnVia ["unexp" ["unexp" "noexp"]]
                                :LearnSupp ["all" ["unexp" "noexp" "noexp-exp" "all"]]
                                :ContrastPreference ["delta" ["delta" "arbitrary"]]
                                :HypPreference ["abd" ["abd" "arbitrary"]]
                                :ConfAdjustment ["max" ["min" "max" "avg" "none"]]
                                :NormalizeSubtype [false [true false]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
