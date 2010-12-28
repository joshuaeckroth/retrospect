(ns simulator.problems.tracking.positions
  (:require [clojure.contrib.math :as math]))

(defrecord Position [x y]
  Object
  (toString [_] (format "(%d,%d)" x y)))

(defn manhattan-distance
 [pos1 pos2]
 (+ (math/abs (- (:x pos1) (:x pos2)))
    (math/abs (- (:y pos1) (:y pos2)))))

