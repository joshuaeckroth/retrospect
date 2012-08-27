(ns retrospect.problems.tracking.movements
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.colors]))

;; Entity movements are stored in a map, whose keys are entity
;; symbols. An entity symbol has metadata key :color. Also, the
;; movements map has metadata keys :width, :height.
;;
;; For each entity, its movements are a vector of maps, where each
;; map, except the first in the vector, has the keys :x, :y, :time,
;; :ox, :oy, :ot; the first map has the keys :x, :y, :time.

(defn new-movements
  [width height]
  (with-meta {} {:width width :height height}))

(defn move-entity
  [movements entity x y time]
  (let [last-pos (last (get movements entity))]
    (update-in movements [entity] conj
               {:ox (:x last-pos) :oy (:y last-pos) :ot (:time last-pos)
                :x x :y y :time time :color (:color last-pos)})))

(defn new-entity
  [movements time]
  (let [[x y] [(my-rand-int (:width (meta movements)))
               (my-rand-int (:height (meta movements)))]
        c (rand-color)
        e (count (keys movements))]
    (assoc movements e [{:x x :y y :time time :color c}])))

(defn entities-at
  [movements x y time]
  (filter (fn [e] (and (< time (count (get movements e)))
                       (let [mov (nth (get movements e) time)]
                         (and (= x (:x mov)) (= y (:y mov))))))
          (keys movements)))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))
       (match-color? (:color det) (:color det2))))

(defn dist
  [x1 y1 x2 y2]
  (double (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))

(defn calc-circle-points
  "From: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm"
  [x0 y0 radius width height]
  (loop [f (- 1.0 radius)
         ddF_x 1
         ddF_y (* -2.0 radius)
         x 0
         y radius
         points [[x0 (+ y0 radius)]
                 [x0 (- y0 radius)]
                 [(+ x0 radius) y0]
                 [(- x0 radius) y0]]]
    (if (>= x y) (filter (fn [[x y]] (and (>= x 0) (>= y 0) (<= x width) (<= y height)))
                   points)
        (let [new-y (if (>= f 0) (dec y) y)
              new-ddF_y (if (>= f 0) (+ 2 ddF_y) ddF_y)
              new-f-tmp (if (>= f 0) (+ f new-ddF_y) f)
              new-x (inc x)
              new-ddF_x (+ 2 ddF_x)
              new-f (+ new-ddF_x new-f-tmp)]
          (recur new-f new-ddF_x new-ddF_y new-x new-y
                 (concat points [[(+ x0 x) (+ y0 y)]
                                 [(- x0 x) (+ y0 y)]
                                 [(+ x0 x) (- y0 y)]
                                 [(- x0 x) (- y0 y)]
                                 [(+ x0 y) (+ y0 x)]
                                 [(- x0 y) (+ y0 x)]
                                 [(+ x0 y) (- y0 x)]
                                 [(- x0 y) (- y0 x)]]))))))

(def loc-distances
  (memoize
   (fn [x y width height]
     (for [nx (range width) ny (range height)]
       [[nx ny] (dist x y nx ny)]))))

(comment
  (defn walk-rand
    [x y mean variance width height]
    (let [d (max 0.0 (my-rand-gauss mean variance))
          points (calc-circle-points x y (int (Math/ceil d)) width height)
          loc-choices (filter (fn [[xx yy]] (and (<= (dist x y xx yy) (+ d 0.1))
                                           (>= (dist x y xx yy) (- d 0.1))))
                         points)]
      (when (not-empty loc-choices)
        (my-rand-nth loc-choices)))))

(defn walk-rand
  [x y mean dists]
  (let [radius (max 0.0 (my-rand-gauss mean 2.0))
        dists2 (map (fn [[xy d]] [xy (Math/abs (- radius d))]) dists)
        closest-dist (apply min (map second dists2))
        loc-choices (map first (filter #(= closest-dist (second %)) dists2))]
    (when (not-empty loc-choices)
      (my-rand-nth loc-choices))))

(defn walk
  "Move an entity maxwalk steps in random directions, respecting angle
   constraints. Also avoid landing in an occupied space."
  [movements entity time mean max-walk]
  (let [width (:width (meta movements))
        height (:height (meta movements))
        movs (reverse (get movements entity))
        last-pos (first movs)
        [ox oy] [(:x last-pos) (:y last-pos)]
        dists (loc-distances ox oy width height)]
    (loop [attempts 0]
      (let [[x y] (walk-rand ox oy mean dists)]
        (cond (= attempts 50)
              ;; ran out of attempts to move to an empty space; just
              ;; stay where we are
              (move-entity movements entity ox oy time)
              ;; make sure we didn't land on an occupied space and did
              ;; not exceed max-walk
              (and x y
                   (empty? (entities-at movements x y time))
                   (empty? (entities-at movements x y (dec time)))
                   (<= (dist ox oy x y) max-walk))
              (move-entity movements entity x y time)
              :else (recur (inc attempts)))))))

(defn entity-movements
  [movements entity mintime maxtime]
  (filter #(and (>= (:time %) mintime) (<= (:time %) maxtime))
          (get movements entity)))

(defn entities
  [movements]
  (keys movements))
