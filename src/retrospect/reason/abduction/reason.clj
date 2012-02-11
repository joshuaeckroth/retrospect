(ns retrospect.reason.abduction.reason
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps analyze init-workspace]])
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
   :default-params-fn (fn [] (:default-params (:abduction @problem)))
   :init-workspace-fn init-workspace
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]
                         ["Depgraph" (depgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-depgraph) (update-hypgraph) (update-logs)))}})
