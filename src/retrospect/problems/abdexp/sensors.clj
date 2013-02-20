(ns retrospect.problems.abdexp.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors :only [init-sensor add-sensed]])
  (:use [loom.graph :only [digraph]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn insertion-noise
  [observations expgraph]
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0)))
    ;; "observe" a random new observation
    (let [[v val] (my-rand-nth (sort (vertex-value-pairs expgraph)))]
      (my-shuffle (sort (conj observations [v val]))))
    ;; otherwise, don't
    observations))

(defn deletion-noise
  [observations]
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDeletionNoise params)) 100.0)))
    ;; remove an observation
    (rest (my-shuffle (sort observations)))
    ;; otherwise, don't
    observations))

(defn distortion-noise
  [observations expgraph]
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDistortionNoise params)) 100.0)))
    ;; change an observation
    (let [obs-shuffled (my-shuffle (sort observations))
          [v val] (first obs-shuffled)
          new-val (my-rand-nth (sort (filter (fn [new-val] (not (#{val} new-val)))
                                        (values expgraph v))))]
      (my-shuffle (conj (rest obs-shuffled) [v new-val])))
    ;; otherwise, don't
    observations))

(defn duplication-noise
  [observations expgraph]
  (if (and (not-empty observations)
           (:Noise params)
           (< (my-rand) (/ (double (:SensorDuplicationNoise params)) 100.0)))
    ;; "observe" a different state on one of the observations
    (let [obs-shuffled (my-shuffle (sort observations))
          [v val] (first obs-shuffled)
          new-val (my-rand-nth (sort (filter (fn [new-val] (not (#{val} new-val)))
                                        (values expgraph v))))]
      (my-shuffle (conj obs-shuffled [v new-val])))
    ;; otherwise, don't
    observations))

(defn sense
  [sensor test time]
  (let [observations (get test (dec time))]
    (add-sensed sensor time (-> observations
                               (insertion-noise (:expgraph (meta sensor)))
                               (deletion-noise)
                               (distortion-noise (:expgraph (meta sensor)))
                               (duplication-noise (:expgraph (meta sensor)))))))

(defn generate-sensors
  [training]
  [(init-sensor "expgraph" sense {:expgraph (:expgraph training)})])
