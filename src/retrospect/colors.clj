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

(defn var-color [x] (Color. x x x))

(defn color-str
  [c]
  (cond (. c equals red) "red"
        (. c equals blue) "blue"
        (. c equals green) "green"
        (. c equals gray) "gray"
        :else "color?"))

(defn match-color?
  [c1 c2]
  (or
   (. c1 equals gray)
   (. c2 equals gray)
   (. c1 equals c2)))
