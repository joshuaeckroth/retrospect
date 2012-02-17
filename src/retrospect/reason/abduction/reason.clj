(ns retrospect.reason.abduction.reason
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps analyze init-workspace init-kb]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.depgraph
         :only [depgraph-tab update-depgraph]])
  (:use [retrospect.reason.abduction.gui.logs
         :only [logs-tab update-logs]])
  (:use [retrospect.state]))

(def reason-abduction
  {:name "Abduction"
   :reason-fn (comp explain add-sensor-hyps)
   :analyze-fn analyze
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :default-params-fn (fn []
                        (merge {:Knowledge [80 [40 60 80 100]]
                                :BelievedKnowledge [80 [40 60 80 100]]
                                :Metareasoning ["NoMetareasoning" ["NoMetareasoning"]]
                                :Threshold [20 [0 10 20 30]]
                                :AnalyzeSensitivity [false [false]]
                                :AnalyzeDeps [false [false]]
                                :ProbPerturb [25 [25]]
                                :UseScores [true [true false]]
                                :Learn [true [true false]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]
                         ["Depgraph" (depgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-depgraph) (update-hypgraph) (update-logs)))}})
