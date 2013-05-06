(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [init-workspace init-kb calc-doubt]])
  (:use [retrospect.reason.abduction.meta
         :only [reason]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp true-meta-hyp?]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.abduction-log
         :only [abduction-log-tab update-abduction-log]])
  (:use [retrospect.reason.abduction.gui.problem-log
         :only [problem-log-tab update-problem-log]])
  (:use [retrospect.state]))

(def reason-abduction
  {:name "Abduction"
   :key :abduction
   :reason-fn reason
   :stats-fn (fn [truedata ors time-now]
               ((:stats-fn (:abduction @problem)) truedata ors time-now))
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :calc-doubt-fn calc-doubt
   :meta-oracle-fn (fn [truedata hyp]
                     (if (= "Abduction" (:name @reasoner))
                       (true-meta-hyp? truedata hyp)
                       false))
   :default-params-fn
   (fn []
     (merge {:Threshold [0 [0]]
             :MetaMinScore [0 [0]]
             :MetaThreshold [0 [0]]
             :MetaHyps ["meta-rej-conflict,meta-rej-minscore,meta-order-dep"
                        ["meta-rej-conflict,meta-rej-minscore,meta-order-dep"]]
             :ScoreMetaHyps ["apriori-diff" ["doubt-diff" "doubt" "apriori-diff"]]
             :EstimateMetaScores [false [true false]]
             :RemoveConflictingRejMinScore [true [true false]]
             :MinScore [0 [0 10]]
             :DoubtIgnoreEssentials [false [true false]]
             :DoubtMeasure ["score-delta-avg" ["score" "delta" "score-delta-prod"
                                               "score-delta-avg" "score-delta-pow"]]
             :DoubtAggregate ["avg" ["geammean" "max" "avg" "min"]]
             :DoubtModifier ["log" ["square" "cube" "sqrt" "log" "none"]]
             :DoubtUnexp [false [true false]]
             :UseScores [true [true]]
             :ScoreLevels [100 [2 3 5 10 100]]
             :ContrastPreference ["delta,score" ["arbitrary" "score,delta"
                                                 "score" "delta" "delta,score"]]
             :HypPreference ["score,expl" ["score" "expl" "score,expl"
                                           "expl,score" "arbitrary"]]
             :ConsiderExplPower [false [true false]]}
            (:default-params (:abduction @problem))))
   :meta-hyp-types #{:meta-rej-minscore :meta-rej-conflict :meta-order-dep}
   :init-workspace-fn init-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Problem Log" (problem-log-tab)]
                        ["Abduction Log" (abduction-log-tab)]
                        ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph)
                              (update-problem-log)
                              (update-abduction-log)))}})
