(ns samre.colors
  (:import (java.awt Color)))

(def red (Color. 204 0 0))

(def red-alpha (Color. 204 0 0 150))

(def blue (Color. 52 101 164))

(def gray (Color. 211 215 207))

(def green (Color. 115 210 22))

(def white (Color. 255 255 255))

(defn var-color [x] (Color. x x x))

(defn color-str
  [c]
  (cond (. c equals red) "red"
        (. c equals blue) "blue"
        (. c equals green) "green"
        :else "color?"))

(defn match-color?
  [o1 o2]
  (or
   (. (:color o1) equals gray)
   (. (:color o2) equals gray)
   (. (:color o1) equals (:color o2))))