(ns retrospect.problems.abdexp.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor expgraph time]
  (add-sensed sensor time expgraph))

(defn generate-sensors
  []
  [(init-sensor "expgraph" sense {})])
