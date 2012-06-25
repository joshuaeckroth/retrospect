(ns retrospect.problems.abdexp.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor test time]
  (add-sensed sensor time (get test time)))

(defn generate-sensors
  []
  [(init-sensor "expgraph" sense {})])
