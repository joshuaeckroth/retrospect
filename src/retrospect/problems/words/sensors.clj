(ns retrospect.problems.words.sensors
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [retrospect.state]))

(defn sense
  [sensor test time]
  (let [symbols (get test (dec time))
        indexes (range (count (get test (dec time))))
        indexed-symbols (partition 2 (interleave symbols indexes))]
    (add-sensed sensor time indexed-symbols)))

(defn generate-sensors
  []
  [(init-sensor "reader" sense {})])

(defn perturb
  [sensor]
  (assoc sensor :sensed
         (reduce (fn [sensed time]
                   (assoc sensed time (get sensed time)))
            (:sensed sensor) (keys (:sensed sensor)))))

