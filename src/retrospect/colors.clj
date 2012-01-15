(ns retrospect.colors
  (:import (java.awt Color)))

(def red (Color. 204 0 0))

(def red-alpha (Color. 204 0 0 100))

(def blue (Color. 52 101 164))

(def gray (Color. 211 215 207))

(def gray-alpha (Color. 211 215 207 100))

(def yellow (Color. 252 233 79))

(def yellow-alpha (Color. 252 233 79 100))

(def green (Color. 115 210 22))

(def white (Color. 255 255 255))

(def black (Color. 0 0 0))

(defn var-color [color degree]
  (reduce (fn [c _] (.brighter c)) color (range degree)))

(defn color-str
  [c]
  (cond (= c red) "red"
        (= c blue) "blue"
        (= c green) "green"
        (= c gray) "gray"
        :else "color?"))

(defn match-color?
  [c1 c2]
  (or
   (= c1 gray)
   (= c2 gray)
   (= c1 c2)))
