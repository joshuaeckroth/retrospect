(ns retrospect.problems.tracking.problem
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate :only
         [evaluate evaluate-comp true-hyp? training-stats]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [generate-sensors perturb]])
  (:use [retrospect.reason.abduction.problems.tracking.hypotheses :only
         [generate-kb make-sensor-hyps hypothesize update-kb]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.tracking.prepared :only
         [prepared-map]])
  (:use [retrospect.state]))

(def tracking-problem
  {:name "Tracking"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn perturb
   :prepared-map prepared-map
   :oracle-fn (fn [truedata hyp]
                (if (= "Abduction" (:name @reasoner))
                    (true-hyp? truedata hyp)
                    false))
   :abduction {:generate-kb-fn generate-kb
               :make-sensor-hyps-fn make-sensor-hyps
               :hypothesize-fn hypothesize
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :update-kb-fn update-kb
               :stats-fn (constantly nil)
               :training-stats-fn training-stats
               :learn-fn (constantly [])
               :hyp-types #{:movement :object :observation}
               :ignore-doubt-types #{:observation}
               :default-params
               {:ResensePrevTime [true [true]]
                :ObjectScore ["max" ["min" "max" "avg"]]}}
   :default-params
   {:Steps [20 [20]]
    :StepsBetween [1 [1]]
    :GridWidth [20 [20]]
    :GridHeight [20 [20]]
    :WalkType ["random" ["random" "gaussian"]]
    :RandomWalkSteps [5 [5]]
    :TrainingSteps [25 [25]]
    :TrainingRandom [0 [0]]
    :GaussianTrueWalkMean [4 [4]]
    :GaussianBelWalkMean [4 [4]]
    :NumberEntities [4 [2 4 6]]
    :SensorSeesColor [60 [0 20 40 60 80 100]]
    :SensorCoverage [100 [100]]}})
