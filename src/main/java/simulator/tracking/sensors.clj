(ns simulator.tracking.sensors
  (:require [simulator.types sensors positions])
  (:import [simulator.types.sensors SensorEntity Sensor])
  (:import [simulator.types.positions Position])
  (:use [simulator.types.sensors :only (sees)])
  (:use [simulator.types.entities :only (pos)])
  (:use [simulator.tracking.grid :only (entity-at)]))

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (Sensor. id left right bottom top []))

(defn update-spotted
  "Create 'spotted' vector based on grid."
  [sensor gridstate]
  (assoc sensor :spotted
	 (map #(SensorEntity. (:time gridstate) (pos %))
	      (filter #(not (nil? %))
		      (for [x (range (:left sensor) (inc (:right sensor)))
			    y (range (:bottom sensor) (inc (:top sensor)))]
			(entity-at (:grid gridstate) (Position. x y)))))))

(defn measure-sensor-coverage
  [width height sensors]
  (let [markers (for [x (range width) y (range height)]
		  (if (some #(sees % x y) sensors) 1 0))
	covered (reduce + markers)]
    (double (* 100 (/ covered (* width height))))))

(defn generate-sensors-sample
  [width height]
  (for [i (range (rand-int (* width height)))]
    (let [left (rand-int width)
	  right (+ left (rand-int (- width left)))
	  bottom (rand-int height)
	  top (+ bottom (rand-int (- height bottom)))]
      (new-sensor "sensorid" left right bottom top))))

(defn generate-sensors-with-coverage
  [width height coverage]
  (loop [sensors (generate-sensors-sample width height)]
    (let [measured (measure-sensor-coverage width height sensors)]
      (if (and (> measured (- coverage 5.0)) (< measured (+ coverage 5.0))) sensors
	  (recur (generate-sensors-sample width height))))))