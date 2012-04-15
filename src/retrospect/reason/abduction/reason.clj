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
   :reason-fn (comp explain add-sensor-hyps)
   :metareasoning-activated?-fn metareasoning-activated?
   :workspace-compare-fn workspace-compare
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :default-params-fn (fn []
                        (merge {:Knowledge [80 [80]]
                                :BelievedKnowledge [80 [80]]
                                :Threshold [0 [0]]
                                :DoubtThreshold [1000 [1000]]
                                :AnalyzeSensitivity [false [false]]
                                :AnalyzeDeps [false [false]]
                                :ProbPerturb [25 [25]]
                                :UseScores [true [true]]
                                :LearnVia ["unexp" ["unexp" "noexp"]]
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
