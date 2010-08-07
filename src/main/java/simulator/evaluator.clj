(ns simulator.evaluator
  (:require [clojure.set :as set :only (intersection)])
  (:use [simulator.types.states :only (get-events)]))

(defn evaluate
  [truestate strat-state]
  (count (set/intersection (set (get-events truestate)) (set (get-events strat-state)))))

(defn correct?
  [truestate event]
  (some #(= event %) (get-events truestate)))