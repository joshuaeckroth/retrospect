(ns retrospect.problems.words.problem
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [evaluate evaluate-comp true-hyp? hyps-equal? stats]])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors perturb]])
  (:use [retrospect.reason.abduction.problems.words.hypotheses :only
         [hypothesize make-sensor-hyps generate-kb]])
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
               :learn-fn (constantly nil)
               :reset-fn (constantly nil)
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :stats-fn stats
               :true-hyp?-fn true-hyp?
               :hyps-equal?-fn hyps-equal?
               :hyp-types [:word :split :merge :merge-noexp]
               :default-params {:HypTypes ["words,biwords"
                                           ["words", "words,biwords"]]}}
   :default-params
   {:Steps [50 [50]]
    :StepsBetween [1 [1]]
    :ResetEachStep [true [true]]
    :SensorNoise [0 [0]]
    ;; ["carroll" "cityu_training" "as_training" "msr_training" "pku_training"]
    :Dataset ["pku_training" ["pku_training"]]
    :MinMergeSplit [10 [0 10 20 30]]
    :DefaultMergeSplit ["merge" ["merge" "split"]]}})
