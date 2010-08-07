(ns simulator.types.positions
  (:require [clojure.contrib.math :as math])
  (:use [simulator.types.generic :only (Printable)]))

(defprotocol PositionMethods
  (equal [this other])
  (manhattan-distance [this other]))

(defrecord Position [x y]
  Printable
  (to-str [this] (format "(%d,%d)" x y))
  PositionMethods
  (equal [this other] (and (= x (:x other)) (= y (:y other))))
  (manhattan-distance
   [this other]
   (+ (math/abs (- x (:x other)))
      (math/abs (- y (:y other))))))

