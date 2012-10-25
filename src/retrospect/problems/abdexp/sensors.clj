(ns retrospect.problems.abdexp.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn insertion-noise
  [observations false-values-map]
  (if (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0))
    (let [false-obs (my-rand-nth (sort (keys false-values-map)))]
      (conj observations [false-obs (false-values-map false-obs)]))
    observations))

(defn deletion-noise
  [observations]
  observations)

(defn distortion-noise
  [observations]
  observations)

(defn sense
  [sensor test time]
  (let [observations (get test time)]
    (add-sensed sensor time (-> observations
                               (insertion-noise (:false-values-map (meta sensor)))
                               (deletion-noise)
                               (distortion-noise)))))

(defn generate-sensors
  [training]
  [(init-sensor "expgraph" sense {:false-values-map (:false-values-map training)})])
