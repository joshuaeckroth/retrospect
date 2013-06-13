(ns retrospect.problems.tracking.colors
  (:import (java.awt Color))
  (:use [geppetto.random]))

(def gray (Color. 211 215 207))

(defn rand-color
  []
  (loop [c (Color. (my-rand-int 256) (my-rand-int 256) (my-rand-int 256))]
    (if (= gray c)
      (recur (Color. (my-rand-int 256) (my-rand-int 256) (my-rand-int 256)))
      c)))

(defn var-color [color degree]
  (reduce (fn [c _] (.brighter c)) color (range degree)))

(defn color-str
  [c]
  (if (= c gray) "gray"
      (format "#%s" (format "%h" (.getRGB c)))))

(defn match-color?
  [c1 c2]
  (or
   (= c1 gray)
   (= c2 gray)
   (= c1 c2)))
