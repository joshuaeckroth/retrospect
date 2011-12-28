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
                :x x :y y :time time :color (:color last-pos) :bias (:bias last-pos)})))

(defn new-entity
  [movements time]
  (let [[x y] [(my-rand-int (:width (meta movements)))
               (my-rand-int (:height (meta movements)))]
        c (my-rand-nth [red blue green])
        b (my-rand-nth [:straight :left :right])
        e (symbol (str (count (keys movements))))]
    (assoc movements e [{:x x :y y :time time :color c :bias b}])))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))
       (match-color? (:color det) (:color det2))))

(defn valid-angle?
  [bias x y ox oy oox ooy]
  (let [theta (Math/atan2 (- oy ooy) (- ox oox))
        cos-mult (Math/cos (- theta))
        sin-mult (Math/sin (- theta))
        nox (- (* cos-mult ox) (* sin-mult oy))
        noy (+ (* sin-mult ox) (* cos-mult oy))
        nx (- (* cos-mult x) (* sin-mult y))
        ny (+ (* sin-mult x) (* cos-mult y))
        ntheta (Math/atan2 (- ny noy) (- nx nox))
        degrees (/ (* ntheta 180.0) 3.1415926)]
    (cond
     ;; angle is between -50 and 50 degrees
     (= bias :straight)
     (and (< -50 degrees) (> 50 degrees))
     ;; angle is between -140 and -40 degrees
     (= bias :left)
     (and (< -140 degrees) (> -40 degrees))
     ;; angle is between 40 and 140 degrees
     (= bias :right)
     (and (< 40 degrees) (> 140 degrees))
     ;; otherwise, no bias, any angle is valid
     :else true)))

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
  (let [width (:width (meta movements))
        height (:height (meta movements))
        movs (reverse (get movements entity))
        last-last-pos (second movs)
        last-pos (first movs)
        bias (:bias last-pos)
        [oox ooy] [(:x last-last-pos) (:y last-last-pos)]
        [ox oy] [(:x last-pos) (:y last-pos)]]
    (loop [attempts 0]
      (let [[x y] (loop [i (my-rand-int (inc maxwalk))
                         loc [ox oy]]
                    (let [[x y] loc
                          [nx ny] (walk-rand loc)]
                      (cond
                       ;; we're done
                       (= i 0) loc
                       ;; if we go out of bounds, don't update pos
                       (or (> nx width) (> ny height) (< nx 0) (< ny 0))
                       (recur (dec i) [x y])
                       ;; more to go
                       :else (recur (dec i) [nx ny]))))]
        (cond (= attempts 50) (move-entity movements entity ox oy time)
              (and (< x (:width (meta movements))) (>= x 0)
                   (< y (:height (meta movements))) (>= y 0)
                   ;; don't check angle if the entity has not made two moves
                   (or (nil? last-last-pos)
                       (valid-angle? bias x y ox oy oox ooy)))
              (move-entity movements entity x y time)
              :else (recur (inc attempts)))))))

(defn entity-movements
  [movements entity mintime maxtime]
  (filter #(and (>= (:time %) mintime) (<= (:time %) maxtime)) (get movements entity)))

(defn entities-at
  [movements x y time]
  (filter (fn [e] (let [mov (nth (get movements e) time)]
                    (and (= x (:x mov)) (= y (:y mov)))))
          (keys movements)))

(defn entities
  [movements]
  (keys movements))

(defn moves-match?
  [mov1 mov2]
  (and (= (:x mov1) (:x mov2)) (= (:y mov1) (:y mov2))
       (= (:ox mov1) (:ox mov2)) (= (:oy mov1) (:oy mov2))
       (= (:time mov1) (:time mov2)) (= (:ot mov1) (:ot mov2))
       (match-color? (:color mov1) (:color mov2))))

(defn dist
  [x1 y1 x2 y2]
  (double (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))
