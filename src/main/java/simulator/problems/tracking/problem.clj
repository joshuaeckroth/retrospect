(ns simulator.problems.tracking.problem
  (:require [simulator problem])
  (:import [simulator.problem Problem])
  (:use [simulator.problems.tracking.evaluate :only (evaluate)])
  (:use [simulator.problems.tracking.truedata :only (generate-truedata)])
  (:use [simulator.problems.tracking.sensors :only (generate-sensors)])
  (:use [simulator.problems.tracking.hypotheses :only
         (generate-hypotheses update-problem-data)])
  (:use [simulator.problems.tracking.player :only
         [player-get-params player-get-params-panel
          player-get-diagram player-update-diagram player-get-stats-panel
          player-update-stats player-update-truedata-log-box]])
  (:use [simulator.problems.tracking.eventlog :only (init-event-log)]))

(def avg-fields [:PercentEventsCorrect :PercentIdentitiesCorrect
		 :NumberEntities :MaxWalk :AvgWalk
		 :ProbNewEntities :GridWidth :GridHeight
                 :SensorCoverage :SensorOverlap])

(def non-avg-fields [])

(def charts
  [{:x :NumberEntities :y :PercentEventsCorrect :name "numes-events"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 5
    :y-range [0.0 100.0]
    :strategy-regression :linear}
   {:x :AvgWalk :y :PercentEventsCorrect :name "avgwalk-events"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 5
    :y-range [0.0 100.0]
    :strategy-regression :linear}
   {:x :NumberEntities :y :PercentIdentitiesCorrect :name "numes-ids"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 5
    :y-range [0.0 100.0]
    :strategy-regression :linear}
   {:x :AvgWalk :y :PercentIdentitiesCorrect :name "avgwalk-ids"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 5
    :y-range [0.0 100.0]
    :strategy-regression :linear}
   {:x :AvgWalk :y :Milliseconds :name "avgwalk-milliseconds"
    :split-by :SensorCoverage :split-list (range 0 101 10) :split-delta 5
    :regression :linear
    :strategy-regression :linear}
   {:x :SensorCoverage :y :PercentEventsCorrect :name "coverage-correct"
    :regression :linear
    :strategy-regression :linear}
   {:x :PercentEventsCorrect :y :PercentIdentitiesCorrect :name "events-identities"
    :split-by :SensorCoverage :split-list (range 10 101 10) :split-delta 5
    :regression :linear}])

(def tracking-problem
  (Problem. "tracking"
            generate-hypotheses
            update-problem-data
            {:get-params-fn player-get-params
             :get-params-panel-fn player-get-params-panel
             :get-diagram-fn player-get-diagram
             :update-diagram-fn player-update-diagram
             :get-stats-panel-fn player-get-stats-panel
             :update-stats-fn player-update-stats
             :update-truedata-log-box-fn player-update-truedata-log-box}
            generate-truedata generate-sensors
            evaluate (init-event-log) avg-fields non-avg-fields charts))
