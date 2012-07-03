(ns retrospect.problems.abdexp.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph]))

(defn sense
  [sensor test time]
  (add-sensed sensor time (forced-nodes (or (get test time) (digraph)))))

(defn generate-sensors
  []
  [(init-sensor "expgraph" sense {})])
