(ns retrospect.problems.abdexp.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn sense
  [sensor test time]
  (let [observations (forced-nodes (or (get test time) (digraph)))]
    (if (= 0 (:SensorNoise params))
      (add-sensed sensor time observations)
      (let [noise-prob (/ (double (:SensorNoise params)) 100.0)
            noisy-observations
            (set/union observations
                   (repeatedly (my-rand-int (int (* (/ (:SensorNoise params) 100)
                                                    (:Steps params))))
                               #(format "O%d" (my-rand-int 1000))))]
        (add-sensed sensor time noisy-observations)))))

(defn generate-sensors
  []
  [(init-sensor "expgraph" sense {})])
