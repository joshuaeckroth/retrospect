(ns retrospect.problems.tracking.problem
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate :only
         [evaluate evaluate-comp true-hyp? hyps-equal?]])
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
   :abduction {:generate-kb-fn generate-kb
               :make-sensor-hyps-fn make-sensor-hyps
               :hypothesize-fn hypothesize
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :update-kb-fn update-kb
               :stats-fn (constantly nil)
               :true-hyp?-fn true-hyp?
               :learn-fn (constantly [])
               :hyps-equal?-fn hyps-equal?
               :hyp-typs [:movement :path :location :bias]
               :default-params
               {:ResensePrevTime [true [true]]}}
   :default-params
   {:Steps [20 [20]]
    :StepsBetween [3 [1 2 3]]
    :SensorNoise [0 [0]]
    :GridWidth [20 [20]]
    :GridHeight [20 [20]]
    :NumberEntities [4 [2 4 6]]
    :MaxWalk [10 [1 2 3 4 5 6 7 8 9 10]]
    :SensorSeesColor [60 [0 20 40 60 80 100]]
    :SensorCoverage [100 [100]]}})
