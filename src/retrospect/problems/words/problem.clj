(ns retrospect.problems.words.problem
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [evaluate evaluate-comp true-hyp? hyps-equal?]])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors perturb]])
  (:use [retrospect.reason.abduction.problems.words.hypotheses :only
         [hypothesize learn make-sensor-hyps generate-kb reset]])
  (:use [retrospect.problems.words.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.words.prepared :only [prepared-map]]))

(def words-problem
  {:name "Words"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn perturb
   :prepared-map prepared-map
   :abduction {:generate-kb-fn generate-kb
               :make-sensor-hyps-fn make-sensor-hyps
               :hypothesize-fn hypothesize
               :learn-fn learn
               :reset-fn reset
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :true-hyp?-fn true-hyp?
               :hyps-equal?-fn hyps-equal?
               :hyp-types [:word :word-seq :learned-word :learned-word-seq]
               :default-params {}}
   :default-params
   {:Steps [10 [10]]
    :StepsBetween [1 [1]]
    :ResetEachStep [true [true]]
    :SensorNoise [0 [0 5 10 15 20]]
    :Dataset ["carroll" ["carroll" "cityu_training" "as_training"
                         "msr_training" "pku_training"]]
    :LearnFeatureSize [3 (range 1 4)]
    :SimMultiplier [200 [100 200 300 400 500]]
    :LengthPenalty [0.3 [0.1 0.3 0.5 0.7]]
    :MaxModelGrams [4 (range 1 6)]}})
