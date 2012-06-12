(ns retrospect.problems.classify.problem
  (:use [retrospect.reason.abduction.problems.classify.evaluate :only
         [evaluate evaluate-comp true-hyp? stats training-stats]])
  (:use [retrospect.problems.classify.truedata :only [generate-truedata]])
  (:use [retrospect.problems.classify.sensors :only [generate-sensors perturb]])
  (:use [retrospect.reason.abduction.problems.classify.hypotheses :only
         [hypothesize make-sensor-hyps generate-kb update-kb]])
  (:use [retrospect.problems.classify.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.classify.prepared :only [prepared-map]]))

(def classify-problem
  {:name "Classify"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log}
   :generate-truedata-fn generate-truedata
   :count-truedata-fn count
   :generate-sensors-fn generate-sensors
   :perturb-fn perturb
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
               :training-stats-fn training-stats
               :true-hyp?-fn true-hyp?
               :hyp-types [:category :catpair-both :catpair-only-left
                           :catpair-only-right :catpair-neither]
               :default-params {}}
   :default-params
   {:Steps [50 [50]]
    :StepsBetween [1 [1]]
    :ResetEachStep [true [true]]
    :ResetSensors [true [true]]
    :GrowEst [false [false]]
    :SensorNoise [0 [0]]
    :Dataset ["spam-bare" ["spam-bare"]]
    :TestIsTraining [false [false]]}})
