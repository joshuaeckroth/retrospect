(ns retrospect.problems.tracking.grid
  (:use [retrospect.colors :only [red blue]])
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
  (flatten grid))

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
  (update-in grid [(grid-pos grid x y)] (fn [v] (filter #(not= e %) v))))

(defn grid-move
  "Move the entity in the grid and update the entity's meta content."
  [grid e x y]
  (let [{ox :x oy :y} (meta e)
        new-e (with-meta e (merge (meta e) {:x x :y y}))]
    (-> grid
        (grid-del ox oy e)
        (grid-add x y new-e))))

(defn rand-pos
  "Generate a random position for a new entity. Returns a map {:x x :y y}."
  [grid]
  {:x (rand-int (:width (meta grid))) :y (rand-int (:height (meta grid)))})

(defn grid-new-entity
  "Create a new entity with a random location."
  [grid time]
  (let [{x :x y :y} (rand-pos grid)
        c (rand-nth [red blue])
        e (with-meta (symbol (str (grid-count grid)))
            {:x x :y y :color c :time time})]
    (grid-add grid x y e)))

(defn find-entity
  [grid entity]
  (find-first #{entity} (grid-entities grid)))

(defn walk1
  "Move an entity one step in a random direction."
  [grid entity]
  (let [e (find-entity grid entity)]
    (loop []
      (let [{ox :x oy :y} (meta e)
            {x :x y :y} (rand-nth [{:x (dec ox) :y oy}
                                   {:x (inc ox) :y oy}
                                   {:x ox :y (inc oy)}
                                   {:x ox :y (dec oy)}
                                   {:x (dec ox) :y (dec oy)}
                                   {:x (dec ox) :y (inc oy)}
                                   {:x (inc ox) :y (dec oy)}
                                   {:x (inc ox) :y (inc oy)}])]
           (if (and (< x (:width (meta grid))) (>= x 0)
                    (< y (:height (meta grid))) (>= y 0))
             (grid-move grid e x y)
             (recur))))))

(defn dist
  [x1 y1 x2 y2]
  (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                (* (- y1 y2) (- y1 y2)))))

(defn update-all-entity-times
  [grid time]
  (with-meta (vec (map (fn [es] (map #(with-meta % (merge (meta %) {:time time})) es)) grid))
    (meta grid)))
