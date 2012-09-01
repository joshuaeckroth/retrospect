(ns retrospect.problems.abdexp.problem
  (:use [retrospect.problems.abdexp.truedata :only [generate-truedata]])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]])
  (:use [retrospect.problems.abdexp.player :only
         [player-get-stats-panel player-update-stats
          player-setup-diagram player-update-diagram
          player-get-truedata-log player-get-problem-log]])
  (:use [retrospect.reason.abduction.problems.abdexp.hypotheses :only
         [generate-kb make-sensor-hyps hypothesize update-kb]])
  (:use [retrospect.reason.abduction.problems.abdexp.evaluate :only
         [evaluate evaluate-comp stats true-hyp?]])
  (:use [retrospect.problems.abdexp.prepared :only [prepared-map]]))

(def abdexp-problem
  {:name "AbdExp"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn identity
   :prepared-map prepared-map
   :abduction {:generate-kb-fn generate-kb
               :make-sensor-hyps-fn make-sensor-hyps
               :hypothesize-fn hypothesize
               :learn-fn (constantly nil)
               :reset-fn (constantly nil)
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :update-kb-fn update-kb
               :stats-fn stats
               :true-hyp?-fn true-hyp?
               :hyp-types [:expl :observation]}
   :default-params
   {:Steps [100 [100]]
    :StepsBetween [1 [1]]
    :NumExplainers [3 [3]]
    :NumExplainsLinks [20 [20]]
    :NumConflictLinks [10 [10]]
    :UniqueGraphs [100 [100]]
    :UniqueTrueSets [5 [5]]
    :FalseAprioriMean [0.4 [0.4]]
    :FalseAprioriVariance [0.3 [0.3]]
    :TrueAprioriMean [0.6 [0.6]]
    :TrueAprioriVariance [0.3 [0.3]]}})
