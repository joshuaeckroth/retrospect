(ns samre.problems.tracking.labels
  (:require [clojure.contrib.math :as math :only [abs ceil]])
  (:use [samre.confidences])
  (:use [samre.colors]))

(defn man-dist
  [x1 y1 x2 y2]
  (+ (math/abs (- x1 x2)) (math/abs (- y1 y2))))

(defn score-distance
  [x1 y1 x2 y2 maxwalk]
  (let [dist (man-dist x1 y1 x2 y2)]
    (cond
     (<= dist (math/ceil (/ maxwalk 4))) PLAUSIBLE
     (<= dist (math/ceil (/ maxwalk 3))) NEUTRAL
     (<= dist (math/ceil (/ maxwalk 2))) IMPLAUSIBLE
     :else ;; equivalent to (<= dist maxwalk)
     VERY-IMPLAUSIBLE)))

(defn match-score
  "spotted is a collection of sensor detections; count-seen is how
  many sensors should have seen the same thing."
  [label spotted count-seen maxwalk]
  (cond
   (= (:color label) gray) NEUTRAL
   :else NEUTRAL))
