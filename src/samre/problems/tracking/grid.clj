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
  [grid e x y]
  (let [{ox :x oy :y} (meta e)]
    (-> grid
        (grid-put ox oy nil)
        (grid-put x y e))))

(defn rand-pos
  "Generate a random position for a new entity. Returns a map {:x x :y y}."
  [grid]
  (let [rand-x #(rand-int (:width (meta grid)))
	rand-y #(rand-int (:height (meta grid)))]
    (loop [px (rand-x)
	   py (rand-y)]
      (if (nil? (grid-at grid x y)) {:x x :y y}
	(recur (rand-x) (rand-y))))))

(defn grid-new-entity
  "Create a new entity with a random (free) location."
  [grid time]
  (if (>= (grid-count grid) (* (:width (meta grid)) (:height (meta grid))))
    grid
    (let [{x :x y :y} (rand-pos grid)
          c (rand-nth [red blue])
          e (with-meta (str (grid-count))
              {:id (hash [x y time]) :x x :y y :color c :time time})]
      (grid-put grid x y e))))

(defn walk1
  "Move an entity one step in a random (free) direction; try to move 4
   times, but give up if no move comes out by that point."
  [grid e]
  (loop [attempts 0]
    (if (< attempts 4)
      (let [{ox :x oy :y} (meta e)
            pos (rand-nth [{:x (dec ox) :y oy}
                           {:x (inc ox) :y oy}
                           {:x ox :y (inc oy)}
                           {:x ox :y (dec oy)}])]
        (if (nil? (grid-at grid (:x pos) (:y pos)))
          (grid-move e (:x pos) (:y pos))
          (recur (inc attempts)))))))

