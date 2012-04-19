(ns retrospect.problems.words.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [retrospect.state]))

(defn sense
  [sensor test time]
  (let [symbol-pairs (partition 2 1 (get test (dec time)))
        indexes (partition 2 1 (range (count (get test (dec time)))))
        indexed-symbol-pairs (partition 2 (interleave symbol-pairs indexes))]
    (add-sensed sensor time indexed-symbol-pairs)))

(defn generate-sensors
  []
  [(init-sensor "reader" sense {})])

(defn perturb
  [sensor]
  (assoc sensor :sensed
         (reduce (fn [sensed time]
                   (assoc sensed time (get sensed time)))
                 (:sensed sensor) (keys (:sensed sensor)))))
