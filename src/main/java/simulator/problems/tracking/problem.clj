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

(def tracking-problem
  (Problem. "tracking" run start-player headers avg-fields non-avg-fields
	    (init-event-log)))
