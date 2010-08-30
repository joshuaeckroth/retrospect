(ns simulator.evaluator
  (:require [clojure.set :as set :only (intersection)])
  (:use [simulator.problems.tracking.eventlog :only (get-events)]))

(defn evaluate
  [trueevents strat-state]
  (let [correct (count (set/intersection (set (get-events trueevents))
					 (set (get-events (:problem-data strat-state)))))
	total (count (get-events trueevents))]
    (double (* 100 (/ correct total)))))
