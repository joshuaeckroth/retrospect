(ns retrospect.problems.tracking.movements
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.state]))

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

(defn entity-movements
  [movements entity mintime maxtime]
  (filter #(and (>= (:time %) mintime) (<= (:time %) maxtime))
          (get movements entity)))

(defn entities
  [movements]
  (keys movements))

(defn dets-match?
  [det det2]
  (and (= (:x det) (:x det2))
       (= (:y det) (:y det2))
       (= (:time det) (:time det2))
       (match-color? (:color det) (:color det2))))

(defn dist
  [x1 y1 x2 y2]
  (if (= "gaussian" (:WalkType params))
    (double (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                          (* (- y1 y2) (- y1 y2)))))
    ;; else, :WalkType = "random"
    ;; use manhattan distance
    (double (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))))

(def loc-distances
  (memoize
   (fn [x y width height]
     (for [nx (range width) ny (range height)]
       [[nx ny] (dist x y nx ny)]))))

(defn walk-gaussian-next
  [x y mean dists]
  (let [radius (max 0.0 (my-rand-gauss mean 2.0))
        dists2 (map (fn [[xy d]] [xy (Math/abs (- radius d))]) dists)
        closest-dist (apply min (map second dists2))
        loc-choices (map first (filter #(= closest-dist (second %)) dists2))]
    (when (not-empty loc-choices)
      (my-rand-nth loc-choices))))

(defn walk-gaussian
  "Move an entity maxwalk steps in random directions, respecting angle
   constraints. Also avoid landing in an occupied space."
  [mean movements entity time]
  (let [width (:width (meta movements))
        height (:height (meta movements))
        movs (reverse (get movements entity))
        last-pos (first movs)
        [ox oy] [(:x last-pos) (:y last-pos)]
        dists (loc-distances ox oy width height)]
    (loop [attempts 0]
      (let [[x y] (walk-gaussian-next ox oy mean dists)]
        (cond (= attempts 50)
              ;; ran out of attempts to move to an empty space; just
              ;; stay where we are
              (move-entity movements entity ox oy time)
              ;; make sure we didn't land on an occupied space
              (and x y
                   (empty? (entities-at movements x y time))
                   (empty? (entities-at movements x y (dec time))))
              (move-entity movements entity x y time)
              :else (recur (inc attempts)))))))

(defn walk-random
  "Brownian motion"
  [walk-steps movements entity time]
  (let [width (:width (meta movements))
        height (:height (meta movements))
        movs (reverse (get movements entity))
        last-pos (first movs)
        [ox oy] [(:x last-pos) (:y last-pos)]]
    (loop [attempts 0]
      (let [[x y] (loop [step 0
                         [x y] [ox oy]]
                    (if (= step walk-steps) [x y]
                        (let [choices (filter (fn [[nx ny]]
                                           (and (>= nx 0) (< nx width)
                                                (>= ny 0) (< ny height)))
                                         [[(dec x) y]
                                          [(inc x) y]
                                          [x (dec y)]
                                          [x (inc y)]])]
                          (if (empty? choices)
                            (recur (inc step) [x y])
                            (recur (inc step) (my-rand-nth choices))))))]
        (cond (= attempts 1)
              (move-entity movements entity ox oy time)
              (and x y
                   (empty? (entities-at movements x y time))
                   (empty? (entities-at movements x y (dec time))))
              (move-entity movements entity x y time)
              :else (recur (inc attempts)))))))

(defn get-walk-fn
  [truedata? random?]
  (if (= "gaussian" (:WalkType params))
    (if random?
      (let [grid-length-avg (/ (double (+ (:GridWidth params) (:GridHeight params))) 2.0)
            mean (* grid-length-avg (my-rand))]
        (partial walk-gaussian mean))
      (if truedata?
        (partial walk-gaussian (:GaussianTrueWalkMean params))
        (partial walk-gaussian (:GaussianBelWalkMean params))))
    (partial walk-random (:RandomWalkSteps params))))

(defn generate-movements
  [movements steps truedata? random?]
  (let [walk-fn (get-walk-fn truedata? random?)]
    (loop [time 1
           m movements]
      (if (> time steps) m
          (recur (inc time) (reduce #(walk-fn %1 %2 time) m (my-shuffle (entities m))))))))
