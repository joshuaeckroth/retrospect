(ns simulator.types.positions
  (:require [clojure.contrib.math :as math]))

(defprotocol PositionMethods
  (equal [this other])
  (manhattan-distance [this other]))

(defrecord Position [x y]
  PositionMethods
  (equal [this other] (and (= (:x this) (:x other)) (= (:y this) (:y other))))
  (manhattan-distance
   [this other]
   (+ (math/abs (- (:x this) (:x other)))
      (math/abs (- (:y this) (:y other))))))

