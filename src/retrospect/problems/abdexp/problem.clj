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
  (:use [retrospect.problems.abdexp.prepared :only [prepared-map]])
  (:use [retrospect.state]))

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
   :oracle-fn (fn [truedata hyp]
                (if (= "Abduction" (:name @reasoner))
                    (true-hyp? truedata hyp)
                    false))
   :abduction {:generate-kb-fn generate-kb
               :make-sensor-hyps-fn make-sensor-hyps
               :hypothesize-fn hypothesize
               :learn-fn (constantly nil)
               :reset-fn (constantly nil)
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :update-kb-fn update-kb
               :stats-fn stats
               :hyp-types #{:expl :expl-composite :observation}
               :ignore-doubt-types #{:observation}
               :default-params {:GetMoreHyps [true [true]]}}
   :default-params
   {:Steps [5 [5]]
    :StepsBetween [1 [1]]
    :NumExplainers [3 [3]]
    :NumExplainsLinks [30 [30]]
    :NumConflictLinks [5 [5]]
    :UniqueGraphs [1000 [1000]]
    :HypScores ["prior" ["prior" "posterior"]]
    :PriorFunc ["max" ["min" "max" "avg"]]
    :MaxStates [3 [2 3 4]]}})
