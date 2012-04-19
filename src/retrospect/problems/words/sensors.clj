(ns retrospect.problems.words.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [retrospect.state]))

(defn sense
  [sensor test time]
  (let [sentence-symbols (partition 2 (interleave (get test (dec time))
                                                  (range (count (get test (dec time))))))]
    (add-sensed sensor time sentence-symbols)))

(defn generate-sensors
  []
  [(init-sensor "reader" sense {})])

(defn perturb
  [sensor]
  (assoc sensor :sensed
         (reduce (fn [sensed time]
                   (assoc sensed time (get sensed time)))
                 (:sensed sensor) (keys (:sensed sensor)))))
