(ns retrospect.problems.aerial.sensors
  (:require [clojure.set :as set])
  (:use [retrospect.sensors])
  (:use [retrospect.state])
  (:use [geppetto.random]))

(defn perturb
  [sensor]
  sensor)

(defn sense
  [sensor frames time]
  ;; keep the doall!
  (let [objs (doall (map (fn [obj]
                           (if (<= (my-rand) (/ (:KeepObjIdProb params) 100.0))
                             obj (dissoc obj :objid)))
                         (:objects (get frames time))))
        objs-above-threshold (filter #(>= (:detscore %) (/ (:SensorThreshold params) 100.0)) objs)]
    (add-sensed sensor time objs-above-threshold)))

(defn generate-sensors
  [training]
  [(init-sensor :main sense {})])
