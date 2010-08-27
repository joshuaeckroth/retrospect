(ns simulator.types.positions
  (:require [clojure.contrib.math :as math]))

(defprotocol PositionMethods
  (equal [this other])
  (manhattan-distance [this other]))

(defrecord Position [x y]
  Object
  (toString [_] (format "(%d,%d)" x y))
  PositionMethods
  (equal [this other] (and (= x (:x other)) (= y (:y other))))
  (manhattan-distance
   [this other]
   (+ (math/abs (- x (:x other)))
      (math/abs (- y (:y other))))))

