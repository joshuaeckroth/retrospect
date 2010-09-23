(ns simulator.problems.circuit.problem
  (:require [simulator.types problem])
  (:import [simulator.types.problem Problem])
  (:use [simulator.problems.circuit.core :only (run)])
  (:use [simulator.problems.circuit.player :only (start-player)]))

(def avg-fields [:Milliseconds
                 :TotalBroken :Correct :Incorrect :Observable :ObservablePercent
                 :PercentCorrect
                 :StrategyCompute :StrategyMilliseconds :StrategyMemory
                 :MinGates :MaxGates :Gates :ProbBroken])

(def non-avg-fields [:Strategy])

(def headers (concat avg-fields non-avg-fields))

(def charts
  [{:x :ProbBroken :y :ObservablePercent :name "probbroken-observable"
    :regression :linear}
   {:x :TotalBroken :y :Correct :name "totalevents-correct"
    :regression :linear}
   {:x :TotalBroken :y :PercentCorrect :name "totalbroken-percentcorrect"
    :regression :linear}
   {:x :Gates :y :Milliseconds :name "gates-milliseconds"
    :regression :linear}
   {:x :Observable :y :PercentCorrect :name "observable-percentcorrect"
    :regression :linear}])

(def circuit-problem
  (Problem. "circuit" run start-player headers avg-fields non-avg-fields charts nil))
