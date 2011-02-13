(ns samre.problems.tracking.grid
  (:use [samre.colors :only [red blue]]))

;; top-left is (0, 0); bottom-right is (width-1, height-1)

(defn new-grid
  "Create a new vector representing the grid. The vector has meta
   attributes :width and :height"
  [width height]
  (with-meta (vec (repeat (* width height) nil)) {:width width :height height}))

(defn grid-entities
  [grid]
  (filter identity grid))

(defn grid-count
  [grid]
  (count (grid-entities grid)))

(defn grid-pos
  [grid x y]
  (+ (* y (:width (meta grid))) x))

(defn grid-at
  [grid x y]
  (nth grid (grid-pos grid x y)))

(defn grid-put
  [grid x y e]
  (assoc grid (grid-pos grid x y) e))

(defn grid-move
  "Move the entity in the grid and update the entity's meta content."
  [grid e x y]
  (let [{ox :x oy :y} (meta e)
        new-e (with-meta e (merge (meta e) {:x x :y y}))]
    (-> grid
        (grid-put ox oy nil)
        (grid-put x y new-e))))

(defn rand-pos
  "Generate a random position for a new entity. Returns a map {:x x :y y}."
  [grid]
  (let [rand-x #(rand-int (:width (meta grid)))
	rand-y #(rand-int (:height (meta grid)))]
    (loop [x (rand-x)
	   y (rand-y)]
      (if (nil? (grid-at grid x y)) {:x x :y y}
	(recur (rand-x) (rand-y))))))

(defn grid-new-entity
  "Create a new entity with a random (free) location."
  [grid time]
  (if (>= (grid-count grid) (* (:width (meta grid)) (:height (meta grid))))
    grid
    (let [{x :x y :y} (rand-pos grid)
          c (rand-nth [red blue])
          e (with-meta (symbol (str (grid-count grid)))
              {:x x :y y :color c :time time})]
      (grid-put grid x y e))))

(defn find-entity
  [grid entity]
  (first (filter (fn [e] (= e entity)) (filter identity grid))))

(defn walk1
  "Move an entity one step in a random (free) direction; try to move 4
   times, but give up if no move comes out by that point."
  [grid entity]
  (let [e (find-entity grid entity)]
    (loop [attempts 0]
      (if (>= attempts 4) grid
        (let [{ox :x oy :y} (meta e)
              {x :x y :y} (rand-nth [{:x (dec ox) :y oy}
                                     {:x (inc ox) :y oy}
                                     {:x ox :y (inc oy)}
                                     {:x ox :y (dec oy)}])]
          (if (and (< x (:width (meta grid)))
                   (>= x 0)
                   (< y (:height (meta grid)))
                   (>= y 0)
                   (nil? (grid-at grid x y)))
            (grid-move grid e x y)
            (recur (inc attempts))))))))
