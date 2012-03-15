(ns retrospect.problems.abdexp.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor expgraphs time]
  (add-sensed sensor time (nth expgraphs (dec time))))

(defn generate-sensors
  []
  [(init-sensor "expgraph" sense {})])
