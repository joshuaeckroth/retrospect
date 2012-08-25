(ns retrospect.problems.abdexp.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn insertion-noise
  [observations]
  (set/union observations
         (set (repeatedly
               (my-rand-int (int (* (/ (:SensorInsertionNoise params) 100)
                                    (:Steps params))))
               #(format "O%d" (my-rand-int 1000))))))

(defn deletion-noise
  [observations]
  (set (filter (fn [_] (>= (my-rand) (/ (double (:SensorDeletionNoise params)) 100.0)))
          observations)))

(defn distortion-noise
  [observations]
  (set (map (fn [v]
            (if (>= (my-rand) (/ (double (:SensorDistortionNoise params)) 100.0))
              v (format "O%d" (my-rand-int 1000))))
          observations)))

(defn sense
  [sensor test time]
  (let [observations (forced-nodes (or (get test time) (digraph)))]
    (add-sensed sensor time (-> observations
                               (insertion-noise)
                               (deletion-noise)
                               (distortion-noise)))))

(defn generate-sensors
  [_]
  [(init-sensor "expgraph" sense {})])
