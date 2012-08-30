(ns retrospect.problems.abdexp.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn insertion-noise
  [observations false-obs]
  (if (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0))
    (conj observations (my-rand-nth (sort false-obs)))
    observations))

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
  (let [observations (get test time)]
    (add-sensed sensor time (-> observations
                               (insertion-noise (:false-obs (meta sensor)))
                               (deletion-noise)
                               (distortion-noise)))))

(defn generate-sensors
  [training]
  [(init-sensor "expgraph" sense {:false-obs (:false-obs training)})])
