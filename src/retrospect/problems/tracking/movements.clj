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

(defn loc-distances
  [x y width height]
  (for [nx (range width) ny (range height)]
    [[nx ny] (dist x y nx ny)]))

(defn walk-rand
  [x y mean variance dists]
  (let [d (my-rand-gauss mean variance)
        dists-sorted (sort-by #(Math/abs (- d (second %))) dists)
        closest-dist (second (first dists-sorted))
        loc-choices (map first (take-while #(= closest-dist (second %)) dists-sorted))]
    (my-rand-nth loc-choices)))

(defn walk
  "Move an entity maxwalk steps in random directions, respecting angle
   constraints. Also avoid landing in an occupied space."
  [movements entity time mean variance]
  (let [width (:width (meta movements))
        height (:height (meta movements))
        movs (reverse (get movements entity))
        last-pos (first movs)
        [ox oy] [(:x last-pos) (:y last-pos)]
        dists (loc-distances ox oy width height)]
    (loop [attempts 0]
      (let [[x y] (walk-rand ox oy mean variance dists)]
        (cond (= attempts 50)
              ;; ran out of attempts to move to an empty space; just
              ;; stay where we are
              (move-entity movements entity ox oy time)
              ;; make sure we didn't land on an occupied space
              (and (empty? (entities-at movements x y time))
                   (empty? (entities-at movements x y (dec time))))
              (move-entity movements entity x y time)
              :else (recur (inc attempts)))))))

(defn entity-movements
  [movements entity mintime maxtime]
  (filter #(and (>= (:time %) mintime) (<= (:time %) maxtime))
          (get movements entity)))

(defn entities
  [movements]
  (keys movements))
