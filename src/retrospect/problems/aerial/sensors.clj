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
                         (:objects (get frames time))))]
    (add-sensed sensor time objs)))

(defn generate-sensors
  [training]
  [(init-sensor :main sense {})])
