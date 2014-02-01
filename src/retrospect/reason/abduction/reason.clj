(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [init-workspace init-kb calc-doubt]])
  (:use [retrospect.reason.abduction.meta
         :only [reason]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp true-meta-hyp?]])
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
             :MetaOracle ["meta-impl-exp,meta-order-dep,meta-conf-exp"
                          ["meta-impl-exp,meta-order-dep,meta-conf-exp"]]
             :MetaHyps ["meta-impl-exp,meta-order-dep,meta-conf-exp"
                        ["meta-impl-exp,meta-order-dep,meta-conf-exp"]]
             :MetaRemainderIgnore [true [true false]]
             :ScoreMetaHyps ["doubt-diff" ["doubt-diff" "doubt" "apriori-diff" "apriori-resolved"]]
             :SimulateSomeMetaHyps [false [true false]]
             :RemoveEssentialConfExp [false [true false]]
             :ScoreMetaConfExp ["opt2" ["opt1" "opt2" "opt3" "opt4" "opt5"]]
             :ScoreMetaImplExp ["opt2" ["opt1" "opt2" "opt3"]]
             :RemoveConflictingImplExp [true [true false]]
             :MaxMetaImplExpScoreDelta [0.75 [0.5 1.0]]
             :MetaEveryNSteps [1 [1 2 3 4 5]]
             :MaxBatch [5 [5]]
             :MinScore [0 [0 10]]
             :AblatePct [0 [0 10]]
             :InvertScoresPct [0 [0 10]]
             :DoubtIgnoreEssentials [false [true false]]
             :DoubtExplainedModifier ["none" ["none" "each" "explained" "min" "max" "avg"]]
             :DoubtMeasure ["weighted-score-delta" ["score-delta-prod" "score-delta-pow"
                                                    "max-score-delta" "min-score-delta"
                                                    "accgraph" "weighted-score-delta"
                                                    "score" "delta" "avg-score-delta-expl"]]
             :DoubtScoreWeight [0.5 [0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]]
             :DoubtAccGraphAgg ["avg" ["min" "max" "avg"]]
             :DoubtAccGraphBranch ["inv-delta" ["mult" "inv-delta"]]
             :DoubtAccGraphBranchMult ["score-delta-prod" ["score" "delta" "score-delta-prod"
                                                           "min-score-delta" "max-score-delta"]]
             :ClearAccGraphSensors [false [true false]]
             :DoubtModifier ["none" ["square" "cube" "sqrt" "log" "none"]]
             :DoubtNoExp [1 [0 1 2 3 4 5]]
             :ReportsHaveScores [true [true false]]
             :UseScores [true [true]]
             :ScoreLevels [100 [2 3 5 10 100]]
             :VirtualScores [false [true false]]
             :VirtualScoresMethod ["gaussian" ["gaussian" "uniform"]]
             :VirtualScoresGoodMean [0.75 [0.75]]
             :VirtualScoresGoodVariance [0.05 [0.05]]
             :VirtualScoresBadMean [0.15 [0.15]]
             :VirtualScoresBadVariance [0.25 [0.25]]
             :VirtualScoresGoodProb [0.75 [0.75]]
             :VirtualScoresBadUpperBound [0.6 [0.6]]
             :VirtualScoresGoodLowerBound [0.4 [0.4]]
             :ContrastPreference ["delta,score" ["arbitrary" "score,delta"
                                                 "score" "delta" "delta,score"]]
             :HypPreference ["score,expl" ["score" "expl" "score,expl"
                                           "expl,score" "arbitrary"]]
             :ConsiderExplPower [false [true false]]
             :NormalizeDelta [true [true false]]
             :GetMoreHypsEveryCycle [false [true false]]}
            (:default-params (:abduction @problem))))
   :meta-hyp-types #{:meta-impl-exp :meta-order-dep :meta-conf-exp}
   :init-workspace-fn init-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Problem Log" (problem-log-tab)]
                         ["Abduction Log" (abduction-log-tab)]])
    :update-tabs-fn (fn [] (do (update-problem-log)
                               (update-abduction-log)))}})
