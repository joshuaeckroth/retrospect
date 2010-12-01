(ns simulator.problems.tracking.problem
  (:require [simulator problem])
  (:import [simulator.problem Problem])
  (:use [simulator.problems.tracking.evaluate :only (evaluate)])
  (:use [simulator.problems.tracking.truedata :only (generate-truedata)])
  (:use [simulator.problems.tracking.sensors :only (generate-sensors)])
  (:use [simulator.problems.tracking.hypotheses :only
         (generate-hypotheses update-problem-data)])
;  (:use [simulator.problems.tracking.player :only (start-player)])
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

(defn single-step
  [ep-state sensors params]
  (let [es (update-problem-data ep-state)]
    (generate-hypotheses es sensors params)))

(def tracking-problem
  (Problem. "tracking"
            single-step nil generate-truedata generate-sensors
            evaluate (init-event-log) avg-fields non-avg-fields charts))
