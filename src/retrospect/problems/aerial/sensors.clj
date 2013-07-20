(ns retrospect.problems.aerial.sensors
  (:use [retrospect.sensors]))

(defn perturb
  [sensor]
  sensor)

(defn sense
  [sensor frames time]
  (add-sensed sensor time (map (fn [obj] (dissoc obj :objid))
                               (:objects (get frames time)))))

(defn generate-sensors
  [training]
  [(init-sensor :main sense {})])
