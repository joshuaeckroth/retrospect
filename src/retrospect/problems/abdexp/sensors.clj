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
           (< (my-rand) (/ (double (:SensorInsertionNoise params)) 100.0)))
    ;; "observe" a different state on some of the observations
    (my-shuffle
     (concat observations
             (for [[v val] (take (my-rand-int (inc (count observations)))
                                 (my-shuffle (sort-by first observations)))]
               [v (my-rand-nth (sort (filter (fn [new-val] (not (#{val} new-val)))
                                        (values expgraph v))))])))
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
