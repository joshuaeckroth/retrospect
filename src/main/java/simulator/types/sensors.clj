(ns simulator.types.sensors
  (:require [simulator.types positions])
  (:import [simulator.types.positions Position])
  (:use [simulator.types.generic :only (Printable to-str)])
  (:use [simulator.types.entities :only (EntityMethods pos)]))

(defrecord SensorEntity [time pos]
  EntityMethods
  (pos [this] (:pos this))
  Printable
  (to-str [this] (format "SensorEntity %s@%d" (to-str pos) time)))

(defprotocol SensorMethods
  (sees [this x y]))

(defrecord Sensor [id left right bottom top spotted]
  SensorMethods
  (sees [this x y] (and (>= x left) (<= x right) (>= y bottom) (<= y top))))

