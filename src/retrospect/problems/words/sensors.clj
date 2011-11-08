(ns retrospect.problems.words.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor truedata time]
  (add-sensed sensor time (get truedata time)))

(defn generate-sensors
  []
  [(init-sensor "reader" sense {})])
