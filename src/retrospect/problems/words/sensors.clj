(ns retrospect.problems.words.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [retrospect.problems.words.truedata :only [add-noise]])
  (:use [retrospect.state]))

(defn sense
  [sensor truedata time]
  (add-sensed sensor time [(get truedata time)]))

(defn generate-sensors
  []
  [(init-sensor "reader" sense {})])

(defn perturb
  [sensor]
  (assoc sensor :sensed
         (reduce (fn [sensed time]
                   (assoc sensed time
                          (first (add-noise [(get sensed time)]
                                            (/ (:ProbPerturb params) 100.0)))))
                 (:sensed sensor) (keys (:sensed sensor)))))
