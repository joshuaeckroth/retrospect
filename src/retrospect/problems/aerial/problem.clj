(ns retrospect.problems.aerial.problem
  (:use [retrospect.problems.aerial.evaluate :only
         [evaluate evaluate-comp true-hyp?]])
  (:use [retrospect.problems.aerial.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.aerial.sensors :only
         [generate-sensors perturb]])
  (:use [retrospect.problems.aerial.hypotheses :only
         [generate-kb make-sensor-hyps hypothesize update-kb]])
  (:use [retrospect.problems.aerial.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.state]))

(def aerial-problem
  {:name "Aerial"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn identity
   :prepared-map {}
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
               :training-stats-fn (constantly nil)
               :learn-fn (constantly [])
               :hyp-types #{:observation :movement}
               :ignore-doubt-types #{:observation}
               :default-params {}}
   :claims []
   :default-params
   {:Steps [5 [5]]
    :StepsBetween [1 [1]]
    :MovementApriori ["dist-diff" ["dist-diff" "dist" "avg-detscores"]]
    :TrainingCount [5 [5]]
    :KeepObjIdProb [10 [10]]
    :Folder ["AA_Easy_01" ["AA_Easy_01"]]
    :SensorThreshold [60 [0 20 40 60 80 100]]}})
