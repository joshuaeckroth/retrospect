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
  (map #(with-meta (:e %) {:x (:x %) :y (:y %) :time (:t %) :color (:color (meta (:e %)))})
       (vals (:movements (meta grid)))))

(defn find-entity
  [grid entity]
  (let [{:keys [x y t color]} (get (:movements (meta grid)) entity)]
    (with-meta entity {:x x :y y :time t :color color})))

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
  (let [{ox :x oy :y t :time} (meta e)
        new-e (with-meta e (merge (meta e) {:x x :y y}))
        old-moves-e (get (:movements (meta grid)) e)
        g (-> grid
              (grid-del ox oy e)
              (grid-add x y new-e))]
    (with-meta g (assoc-in (meta g) [:movements e] (assoc old-moves-e :x x :y y)))))

(defn rand-pos
  "Generate a random position for a new entity. Returns a map {:x x :y y}."
  [grid]
  {:x (my-rand-int (:width (meta grid))) :y (my-rand-int (:height (meta grid)))})

(defn grid-new-entity
  "Create a new entity with a (possibly) random location and random color."
  [grid time]
  (let [{x :x y :y} (rand-pos grid)
        c (my-rand-nth [red blue green])
        e (with-meta (symbol (str (grid-count grid)))
            {:x x :y y :color c :time time})
        g (with-meta grid (assoc-in (meta grid) [:movements e]
                                    {:e e :ox x :oy y :ot time :x x :y y :t time}))]
    (grid-add g x y e)))

(defn walk1
  "Move an entity one step in a random direction."
  [grid entity]
  (let [e (find-entity grid entity)]
    (loop []
      (let [{ox :x oy :y} (meta e)
            {x :x y :y} (my-rand-nth [{:x ox :y oy} ;; don't move
                                      {:x (dec ox) :y oy}
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
  (double (math/sqrt (+ (* (- x1 x2) (- x1 x2))
                        (* (- y1 y2) (- y1 y2))))))

(defn update-all-entities
  [grid time]
  (let [moves (:movements (meta grid))
        new-moves (reduce #(assoc %1 %2 {:e %2
                                         :ox (:x (get moves %2))
                                         :oy (:y (get moves %2))
                                         :ot (:t (get moves %2))
                                         :x (:x (get moves %2))
                                         :y (:y (get moves %2))
                                         :t time})
                          {} (keys moves))]
    (with-meta (vec (map (fn [es] (vec (map #(with-meta
                                               % (merge (meta %) {:time time}))
                                            es))) grid))
      (assoc (meta grid) :movements new-moves))))
