(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [reset-workspace init-workspace init-kb calc-doubt calc-coverage]])
  (:use [retrospect.reason.abduction.meta
         :only [reason metareason]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.logs
         :only [logs-tab update-logs]])
  (:use [retrospect.state]))

(def reason-abduction
  {:name "Abduction"
   :reason-fn reason
   :stats-fn (fn [truedata ors time-now]
               ((:stats-fn (:abduction @problem)) truedata ors time-now))
   :metareason-fn metareason
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :calc-doubt-fn calc-doubt
   :calc-coverage-fn calc-coverage
   :default-params-fn
   (fn []
     (merge {:Threshold [0 [0]]
             :MinApriori [10 [10]]
             :UseScores [true [true]]
             :MetaMinApriori [20 [20]]
             :MetaBatchAttempts [3 [3]]
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
