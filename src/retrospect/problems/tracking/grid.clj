(ns retrospect.problems.tracking.grid
  (:use [retrospect.random])
  (:use [retrospect.colors :only [red blue green]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:require [clojure.contrib.math :as math]))

;; top-left is (0, 0); bottom-right is (width-1, height-1)

(defn new-grid
  "Create a new vector representing the grid. The vector has meta
   attributes :width and :height"
  [width height]
  (with-meta (vec (repeat (* width height) [])) {:width width :height height}))

(defn grid-entities
  [grid]
  (sort-by :e (map #(with-meta (:e %) {:x (:x %) :y (:y %) :ox (:ox %) :oy (:oy %)
                                       :time (:time %) :color (:color (meta (:e %)))})
                   (vals (:movements (meta grid))))))

(defn find-entity
  [grid entity]
  (let [{:keys [x y ox oy time]} (get (:movements (meta grid)) entity)]
    (with-meta entity {:x x :y y :ox ox :oy oy :time time})))

(defn grid-count
  [grid]
  (count (grid-entities grid)))

(defn grid-pos
  [grid x y]
  (+ (* y (:width (meta grid))) x))

(defn grid-at
  [grid x y]
  (nth grid (grid-pos grid x y)))

(defn grid-add
  [grid x y e]
  (update-in grid [(grid-pos grid x y)] conj e))

(defn grid-del
  [grid x y e]
  (update-in grid [(grid-pos grid x y)] (fn [v] (vec (filter #(not= e %) v)))))

(defn grid-move
  "Move the entity in the grid and update the entity's meta content."
  [grid e x y]
  (let [{ox :x oy :y t :time c :color} (meta e)
        new-e (with-meta e (merge (meta e) {:x x :y y}))
        old-moves-e (get (:movements (meta grid)) e)
        g (-> grid
              (grid-del ox oy e)
              (grid-add x y new-e))]
    (with-meta g (assoc-in (meta g) [:movements e]
                           (assoc old-moves-e :x x :y y :ox ox :oy oy)))))

(defn rand-pos
  "Generate a random position for a new entity. Returns a map {:x x :y y}."
  [grid]
  (let [x (my-rand-int (:width (meta grid)))
        y (my-rand-int (:height (meta grid)))]
    {:x x :y y}))

(defn grid-new-entity
  "Create a new entity with a random location and random color."
  [grid time]
  (let [{x :x y :y} (rand-pos grid)
        c (my-rand-nth [red blue green])
        e (with-meta (symbol (str (grid-count grid)))
            {:x x :y y :color c :time time})
        g (with-meta grid (assoc-in (meta grid) [:movements e]
                                    {:e e :ox x :oy y :ot time :x x :y y :time time}))]
    (grid-add g x y e)))

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
  [grid entity maxwalk]
  (let [e (find-entity grid entity)]
    (loop []
      (let [[ox oy oox ooy] [(:x (meta e)) (:y (meta e)) (:ox (meta e)) (:oy (meta e))]
            [x y] (loop [i (my-rand-int (inc maxwalk))
                         loc [ox oy]]
                    (if (= i 0) loc
                        (recur (dec i) (walk-rand loc))))
            angle (calc-angle x y ox oy oox ooy)]
        (if (and (< x (:width (meta grid))) (>= x 0)
                 (< y (:height (meta grid))) (>= y 0)
                 ;; angle is greater than 135-degrees
                 (<= (/ (* 135 3.1415926) 180.0) angle))
          (grid-move grid e x y)
          (recur))))))

(defn dist
  [x1 y1 x2 y2]
  (double (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))

(defn update-all-entities
  [grid time]
  (let [moves (:movements (meta grid))
        new-moves (reduce #(assoc %1 %2 {:e %2
                                         :ox (:ox (get moves %2))
                                         :oy (:oy (get moves %2))
                                         :ot (:ot (get moves %2))
                                         :x (:x (get moves %2))
                                         :y (:y (get moves %2))
                                         :time time})
                          {} (keys moves))]
    (with-meta (vec (map (fn [es] (vec (map #(with-meta
                                               % (merge (meta %) {:time time}))
                                            es))) grid))
      (assoc (meta grid) :movements new-moves))))
