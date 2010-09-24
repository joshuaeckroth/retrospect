(ns simulator.problems.tracking.problem
  (:require [simulator.types problem])
  (:import [simulator.types.problem Problem])
  (:use [simulator.problems.tracking.core :only (run)])
  (:use [simulator.problems.tracking.player :only (start-player)])
  (:use [simulator.problems.tracking.eventlog :only (init-event-log)]))

(def avg-fields [:Milliseconds :PercentCorrect
		 :StrategyCompute :StrategyMilliseconds :StrategyMemory
		 :Steps :NumberEntities :MaxWalk :AvgWalk
		 :ProbNewEntities :SensorReportNoise :BeliefNoise
		 :GridWidth :GridHeight :SensorCoverage :SensorOverlap])

(def non-avg-fields [:Strategy])

(def headers (concat avg-fields non-avg-fields))

(def charts
  [{:x :NumberEntities :y :PercentCorrect :name "numberentities-percentcorrect"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :y-range [0.0 100.0]}
   {:x :AvgWalk :y :Milliseconds :name "avgwalk-milliseconds"
    :split-by :SensorCoverage :split-list (range 0 100 10) :split-delta 5
    :regression :linear}
   {:x :SensorCoverage :y :PercentCorrect :name "coverage-correct"
    :regression :linear}])

(def tracking-problem
  (Problem. "tracking" run start-player headers avg-fields non-avg-fields charts
	    (init-event-log)))
