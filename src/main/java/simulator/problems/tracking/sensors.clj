(ns simulator.problems.tracking.sensors
  (:require [simulator.problems.tracking positions])
  (:import [simulator.problems.tracking.positions Position])
  (:use [simulator.types.hypotheses :only (Hypothesis)])
  (:use [simulator.confidences])
  (:use [simulator.problems.tracking.entities :only (EntityMethods pos)])
  (:use [simulator.problems.tracking.grid :only (entity-at)]))

(defrecord SensorEntity [id apriori time pos]
  Hypothesis
  (get-id [_] id)
  (get-apriori [_] apriori)
  EntityMethods
  (pos [this] (:pos this))
  Object
  (toString [_] (format "SensorEntity %s (a=%d) %s@%d" id apriori (str pos) time)))

(defn make-sensorentity-id
  [pos time]
  (format "SE%d%d%d" (:x pos) (:y pos) time))

(defprotocol SensorMethods
  (sees [this x y]))

(defrecord Sensor [id left right bottom top spotted]
  SensorMethods
  (sees [this x y] (and (>= x left) (<= x right) (>= y bottom) (<= y top))))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (Sensor. id left right bottom top []))

(defn update-spotted
  "Create 'spotted' vector based on grid."
  [sensor gridstate]
  (assoc sensor :spotted
	 (doall (map #(SensorEntity. (make-sensorentity-id (pos %) (:time gridstate))
                                     VERY-PLAUSIBLE (:time gridstate) (pos %))
                     (filter #(not (nil? %))
                             (doall (for [x (range (:left sensor) (inc (:right sensor)))
                                          y (range (:bottom sensor) (inc (:top sensor)))]
                                      (entity-at (:grid gridstate) (Position. x y)))))))))

(defn measure-sensor-coverage
  [width height sensors]
  (let [markers (for [x (range width) y (range height)]
		  (if (some #(sees % x y) sensors) 1 0))
	covered (reduce + markers)]
    (double (* 100 (/ covered (* width height))))))

(defn generate-sensors-sample
  [width height]
  (doall (for [i (range (rand-int (* width height)))]
           (let [left (rand-int width)
                 right (+ left (rand-int (- width left)))
                 bottom (rand-int height)
                 top (+ bottom (rand-int (- height bottom)))]
             (new-sensor "sensorid" left right bottom top)))))

(defn generate-sensors-with-coverage
  [width height coverage]
  (loop [sensors (generate-sensors-sample width height)]
    (let [measured (measure-sensor-coverage width height sensors)]
      (if (and (> measured (- coverage 5.0)) (< measured (+ coverage 5.0))) sensors
	  (recur (generate-sensors-sample width height))))))

(defn measure-sensor-overlap
  [width height sensors]
  (let [count-xy
	(doall (for [x (range width) y (range height)]
                 (count (filter identity (map (fn [s] (sees s x y)) sensors)))))]
    (double (/ (reduce + count-xy) (* width height)))))
