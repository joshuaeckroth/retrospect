(ns simulator.strategies
  (:require [simulator.types states])
  (:import [simulator.types.states State]))

(defn init-strat-state
  [strategy]
  (case strategy
	"guess" (State. [] [] [])
	"nearest" (State. [] [] [])))
