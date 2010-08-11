(ns simulator.evaluator
  (:require [clojure.set :as set :only (intersection)])
  (:use [simulator.types.states :only (get-events)]))

(defn evaluate
  [truestate strat-state]
  (let [correct (count (set/intersection (set (get-events truestate))
					 (set (get-events strat-state))))
	total (count (get-events truestate))]
    (double (* 100 (/ correct total)))))
