(ns retrospect.problems.causal.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor truedata time]
  (add-sensed sensor time (nth (:observed-seq truedata) time)))

(defn generate-sensors
  []
  [(init-sensor :main sense {})])
