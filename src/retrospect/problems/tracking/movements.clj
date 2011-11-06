(ns retrospect.problems.tracking.movements
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.random])
  (:use [retrospect.colors]))

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
        c (my-rand-nth [red blue green])
        e (symbol (str (count (keys movements))))]
    (assoc movements e [{:x x :y y :time time :color c}])))

(defn calc-angle
  [x y ox oy oox ooy]
  (let [[dx1 dy1] [(- oox ox) (- ooy oy)]
        [dx2 dy2] [(- x ox) (- y oy)]]
    (if (or (and (= 0 dx1) (= 0 dy1))
            (and (= 0 dx2) (= 0 dy2)))
      ;; if no movement, just say it's 180-degrees
      3.1415926
      (Math/acos (/ (+ (* dx1 dx2) (* dy1 dy2))
                    (* (Math/sqrt (+ (* dx1 dx1) (* dy1 dy1)))
                       (Math/sqrt (+ (* dx2 dx2) (* dy2 dy2)))))))))

(defn valid-angle?
  [angle]
  ;; angle is greater than 135-degrees
  (<= (/ (* 135 3.1415926) 180.0) angle))

(defn walk-rand
  [[x y]]
  (my-rand-nth [[x y] ;; don't move
                [(dec x) y]
                [(inc x) y]
                [x (inc y)]
                [x (dec y)]
                [(dec x) (dec y)]
                [(dec x) (inc y)]
                [(inc x) (dec y)]
                [(inc x) (inc y)]]))

(defn walk
  "Move an entity maxwalk steps in random directions, respecting angle constraints."
  [movements entity time maxwalk]
  (loop []
    (let [movs (reverse (get movements entity))
          last-last-pos (second movs)
          last-pos (first movs)
          [x y] (loop [i (my-rand-int (inc maxwalk))
                       loc [(:x last-pos) (:y last-pos)]]
                  (if (= i 0) loc
                      (recur (dec i) (walk-rand loc))))]
      (if (and (< x (:width (meta movements))) (>= x 0)
               (< y (:height (meta movements))) (>= y 0)
               ;; don't check angle if the entity has not made two moves
               (or (nil? last-last-pos)
                   (valid-angle? (calc-angle x y (:x last-pos) (:y last-pos)
                                             (:x last-last-pos) (:y last-last-pos)))))
        (move-entity movements entity x y time)
        (recur)))))

(defn entity-movements
  [movements entity mintime maxtime]
  (filter #(and (>= (:time %) mintime) (<= (:time %) maxtime)) (get movements entity)))

(defn entities-at
  [movements x y time]
  (filter (fn [e] (some (fn [mov] (and (= x (:x mov)) (= y (:y mov)) (= time (:time mov))))
                        (get movements e)))
          (keys movements)))

(defn entities
  [movements]
  (keys movements))

(defn dist
  [x1 y1 x2 y2]
  (double (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))
