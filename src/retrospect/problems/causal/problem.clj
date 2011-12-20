(ns retrospect.problems.causal.problem
  (:require [retrospect.problem])
  (:import [retrospect.problem Problem])
  (:use [loom.graph :only [nodes]])
  (:use [loom.attr :only [remove-attr]])
  (:use [retrospect.problems.causal.evaluate :only
         [evaluate evaluate-comparative true-hyp?]])
  (:use [retrospect.problems.causal.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.causal.sensors :only
         [generate-sensors]])
  (:use [retrospect.problems.causal.hypotheses :only
         [hypothesize commit-decision retract no-explainer-hyps]])
  (:use [retrospect.problems.causal.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.causal.monitor :only
         [monitor]])
  (:use [retrospect.problems.causal.prepared :only
         [prepared-map]])
  (:use [retrospect.problems.causal.javabayes :only
         [unobserve-all]])
  (:use [retrospect.state]))

(defn generate-problem-data
  [truedata sensors]
  {:network (:network truedata)
   :explanation-nodes (:explanation-nodes truedata)
   :believed {}})

(def causal-problem
  (Problem. "Causal network"
            monitor
            {:get-stats-panel-fn player-get-stats-panel
             :update-stats-fn player-update-stats
             :get-truedata-log player-get-truedata-log
             :get-problem-log player-get-problem-log
             :setup-diagram-fn player-setup-diagram
             :update-diagram-fn player-update-diagram}
            generate-truedata
            generate-sensors
            prepared-map
            hypothesize
            identity ;; get-more-hyps
            commit-decision
            retract
            generate-problem-data
            (constantly []) ;; inconsistent
            no-explainer-hyps
            evaluate
            evaluate-comparative
            true-hyp?
            {:Steps 25
             :Threshold 20
             :StepsBetween 6
             :SensorNoise 0
             :BeliefNoise 0
             :ObservableNodes 10
             :InternalNodes 10
             :MetaReasoning "NoMetareasoning"
             :TransitiveExplanation false}))
