(ns simulator.tracking.sensors
  (:require [simulator.types sensors positions])
  (:import [simulator.types.sensors SensorEntity Sensor])
  (:import [simulator.types.positions Position])
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


(defn generate-sensors-with-coverage
  [width height coverage]
  (let [area (* width height)
	to-cover (int (* (/ coverage 101) area)) ; use 101 in denom to ensure top < height
	left 0
	right (dec (if (< to-cover width) to-cover width))
	bottom 0
	top (int (/ to-cover width))]
    [(new-sensor "X" left right bottom top)]))