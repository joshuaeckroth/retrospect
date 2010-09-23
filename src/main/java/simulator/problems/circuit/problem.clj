(ns simulator.problems.circuit.problem
  (:require [simulator.types problem])
  (:import [simulator.types.problem Problem])
  (:use [simulator.problems.circuit.core :only (run)])
  (:use [simulator.problems.circuit.player :only (start-player)]))

(def avg-fields [:Milliseconds
                 :TotalEvents :Correct :Incorrect :Observable :PercentCorrect
                 :StrategyCompute :StrategyMilliseconds :StrategyMemory
                 :MinGates :MaxGates :ProbBroken])

(def non-avg-fields [:Strategy])

(def headers (concat avg-fields non-avg-fields))

(def circuit-problem
  (Problem. "circuit" run start-player headers avg-fields non-avg-fields nil))
