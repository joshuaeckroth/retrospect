(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace init-kb
                update-hypotheses update-kb reset-workspace
                calc-doubt calc-coverage]])
  (:use [retrospect.reason.abduction.meta
         :only [metareason metareasoning-activated? workspace-better?]])
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
                    (update-kb (explain (update-hypotheses
                                         (add-sensor-hyps ws time-prev time-now sensors)
                                         time-now)))
                    (update-kb (explain (update-hypotheses ws time-now))))))
   :stats-fn (fn [truedata ors time-now]
               ((:stats-fn (:abduction @problem)) truedata ors time-now))
   :metareasoning-activated?-fn metareasoning-activated?
   :metareason-fn (fn [truedata est time-prev time-now sensors]
                    (if (not= "abd" (:Metareasoning params))
                      {:est est :considered? false :accepted-branch? false}
                      (metareason truedata est time-prev time-now sensors)))
   :workspace-better?-fn workspace-better?
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :calc-doubt-fn calc-doubt
   :calc-coverage-fn calc-coverage
   :default-params-fn (fn []
                        (merge {:Threshold [0 [0]]
                                :UseScores [true [true]]
                                :ContrastPreference ["delta" ["delta" "arbitrary" "apriori,delta"]]
                                :HypPreference ["abd" ["abd" "arbitrary"]]
                                :ConsiderExplPower [false [true false]]
                                :TransitiveExplanation [false [true false]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :reset-workspace-fn reset-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                        ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
