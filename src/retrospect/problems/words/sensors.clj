(ns retrospect.problems.words.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]]))

(defn sense
  [sensor letter time]
  (println sensor letter time)
  (add-sensed sensor time letter))

(defn generate-sensors
  [params]
  [(init-sensor "reader" sense {})])
