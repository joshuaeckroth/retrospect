(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [init-workspace init-kb calc-doubt calc-coverage]])
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
     (merge {:Exhaustive [false [false]]
             :Threshold [0 [0]]
             :MetaThreshold [0 [0]]
             :MetaHyps ["rej-conflict,lower-minapriori,order-dep"
                        ["rej-conflict,lower-minapriori,order-dep"]]
             :MinApriori [0 [0 10]]
             :DoubtMeasure ["delta" ["score" "delta"]]
             :DoubtAggregate ["avg" ["max" "avg"]]
             :UseScores [true [true]]
             :ScoreLevels [100 [2 3 5 10 100]]
             :ContrastPreference ["score,delta" ["arbitrary" "score,delta"
                                                 "score" "delta" "delta,score"]]
             :HypPreference ["expl,score" ["score" "expl" "score,expl"
                                           "expl,score" "arbitrary"]]
             :ConsiderExplPower [false [true false]]}
            (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                        ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
