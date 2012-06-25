(ns retrospect.problems.abdexp.problem
  (:use [retrospect.problems.abdexp.truedata :only [generate-truedata]])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]])
  (:use [retrospect.problems.abdexp.player :only
         [player-get-stats-panel player-setup-diagram player-update-diagram]])
  (:use [retrospect.reason.abduction.problems.abdexp.hypotheses :only
         [generate-kb make-sensor-hyps hypothesize update-kb]])
  (:use [retrospect.reason.abduction.problems.abdexp.evaluate :only
         [evaluate evaluate-comp stats true-hyp?]]))

(def abdexp-problem
  {:name "AbdExp"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn (constantly nil)
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram
                :get-truedata-log (constantly "")
                :get-problem-log (constantly "")}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn identity
   :prepared-map {}
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
               :hyp-types []}
   :default-params {:Steps [100 [100]]
                    :StepsBetween [1 [1]]
                    :ResetEachStep [false [false]]
                    :GrowEst [true [true]]
                    :SensorNoise [0 [0]]
                    :NumVertices [40 [40]]
                    :MaxExplainLinks [10 [10]]
                    :MaxConflictLinks [10 [10]]}})
