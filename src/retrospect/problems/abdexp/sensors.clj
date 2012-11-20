(ns retrospect.problems.abdexp.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn insertion-noise
  [observations expgraph]
  (if (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0))
    ;; if we're adding noise, grab a random vertex and random value
    (let [v (my-rand-nth (sort (vertices expgraph)))]
      (conj observations [v (my-rand-nth (sort (values expgraph v)))]))
    observations))

(defn deletion-noise
  [observations]
  observations)

(defn distortion-noise
  [observations]
  observations)

(defn sense
  [sensor test time]
  (let [observations (get test (dec time))]
    (add-sensed sensor time (-> observations
                               (insertion-noise (:expgraph (meta sensor)))
                               (deletion-noise)
                               (distortion-noise)))))

(defn generate-sensors
  [training]
  [(init-sensor "expgraph" sense {:expgraph (:expgraph training)})])
