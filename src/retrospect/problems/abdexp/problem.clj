(ns retrospect.problems.abdexp.problem
  (:use [retrospect.problems.abdexp.truedata :only [generate-truedata]])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]])
  (:use [retrospect.problems.abdexp.player :only
         [player-get-stats-panel player-update-stats
          player-setup-diagram player-update-diagram
          player-get-truedata-log player-get-problem-log]])
  (:use [retrospect.problems.abdexp.hypotheses :only
         [generate-kb make-sensor-hyps hypothesize update-kb]])
  (:use [retrospect.problems.abdexp.evaluate :only
         [evaluate evaluate-comp stats true-hyp?]])
  (:use [retrospect.problems.abdexp.prepared :only [prepared-map]])
  (:use [retrospect.problems.abdexp.claims])
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
               :find-noise-hyps-fn (fn [hyps]
                                     ;; collect all obs and equivalent
                                     ;; expl; use the expl if both the
                                     ;; obs and expl are present for
                                     ;; the same vertex-value pair
                                     (let [obs (filter #(= :observation (:type %)) hyps)
                                           obs-vertex-values (set (map (fn [h] [(:vertex h) (:value h)]) obs))
                                           expl (filter #(and (= :expl (:subtype %))
                                                              (obs-vertex-values [(:vertex %) (:value %)]))
                                                        hyps)]
                                       (vals (reduce (fn [m h] (assoc m [(:vertex h) (:value h)] h))
                                                     {} (concat obs expl)))))
               :hyp-types #{:expl :observation}
               :ignore-doubt-types #{:observation}
               :default-params {:GetMoreHyps [true [true]]
                                :OnlySingleExplainers [false [true false]]
                                :OnlyCompleteExplainers [false [true false]]
                                :MetaRemainderIgnore [false [false]]}
               :claims abduction-claims}
   :claims abduction-claims
   :default-params
   {:Steps [1 [1]]
    :StepsBetween [1 [1]]
    :NumExplainers [3 [3]]
    :NumExplainsLinks [40 [40]]
    :NumConflictLinks [6 [6]]
    :UniqueGraphs [1000 [1000]]
    :SensorSubset [100.0 [0.0 25.0 50.0 75.0 100.0]]
    :HypScores ["posterior" ["prior" "posterior"]]
    :PriorFunc ["max" ["min" "max" "avg"]]
    :MaxStates [2 [2 3 4]]
    :BestProbMult [3.0 [1.0 2.0 3.0]]}})

