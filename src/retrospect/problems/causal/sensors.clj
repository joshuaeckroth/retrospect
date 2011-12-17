(ns retrospect.problems.causal.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor trudata time]
  sensor)

(defn generate-sensors
  []
  [(init-sensor :main sense {})])
