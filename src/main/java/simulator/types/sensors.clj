(ns simulator.types.sensors
  (:require [simulator.types positions])
  (:import [simulator.types.positions Position])
  (:use [simulator.types.generic :only (Printable)])
  (:use [simulator.types.entities :only (EntityMethods pos)]))

(defrecord SensorEntity [time pos]
  EntityMethods
  (pos [this] (:pos this))
  Printable
  (toStr [this] (format "SensorEntity (%d,%d)@%d" (:x (:pos this)) (:y (:pos this)) (:time this))))

(defrecord Sensor [id left right bottom top spotted])

