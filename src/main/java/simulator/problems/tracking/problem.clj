(ns simulator.problems.tracking.problem
  (:require [simulator.types problem])
  (:import [simulator.types.problem Problem])
  (:use [simulator.problems.tracking.core :only (run)])
  (:use [simulator.problems.tracking.player :only (start-player)])
  (:use [simulator.problems.tracking.eventlog :only (init-event-log)]))

(def avg-fields [:Milliseconds :PercentEventsCorrect :PercentIdentitiesCorrect
		 :StrategyCompute :StrategyMilliseconds :StrategyMemory
		 :Steps :NumberEntities :MaxWalk :AvgWalk
		 :ProbNewEntities :SensorReportNoise :BeliefNoise
		 :GridWidth :GridHeight :SensorCoverage :SensorOverlap])

(def non-avg-fields [:Strategy])

(def headers (concat avg-fields non-avg-fields))

(def charts
  [{:x :NumberEntities :y :PercentEventsCorrect :name "numes-events"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :y-range [0.0 100.0]}
   {:x :AvgWalk :y :PercentEventsCorrect :name "avgwalk-events"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :y-range [0.0 100.0]}
   {:x :NumberEntities :y :PercentIdentitiesCorrect :name "numes-ids"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :y-range [0.0 100.0]}
   {:x :AvgWalk :y :PercentIdentitiesCorrect :name "avgwalk-ids"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :y-range [0.0 100.0]}
   {:x :AvgWalk :y :Milliseconds :name "avgwalk-milliseconds"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :regression :linear}
   {:x :SensorCoverage :y :PercentEventsCorrect :name "coverage-correct"
    :regression :linear}
   {:x :PercentEventsCorrect :y :PercentIdentitiesCorrect :name "events-identities"
    :regression :linear}])

(def tracking-problem
  (Problem. "tracking" run start-player headers avg-fields non-avg-fields charts
	    (init-event-log)))
