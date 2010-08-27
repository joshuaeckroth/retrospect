(ns simulator.problems.tracking.grid
  (:require [simulator.types positions entities])
  (:import [simulator.types.positions Position])
  (:import [simulator.types.entities Entity EntitySnapshot])
  (:use [simulator.types.entities :only (pos add-snapshot)])
  (:use [simulator.types.positions :only (equal)]))

(defn get-grid-pos
  [grid pos]
  (+ (* (:y pos) (:width grid)) (:x pos)))

(defn replace-entity
  "Replace an entity in the grid (which may be nil) with a new one."
  [grid oldentity newentity]
  (let [snapshot (last (:snapshots newentity))
	newgrid (assoc grid :gridvec
		       (assoc (:gridvec grid)
			 (get-grid-pos grid (:pos snapshot)) newentity))]
    (if (nil? oldentity) newgrid
	(let [prior-snapshot (last (:snapshots oldentity))]
	  (if (equal (:pos prior-snapshot) (:pos snapshot)) newgrid
	      (assoc newgrid :gridvec
		     (assoc (:gridvec newgrid)
		       (get-grid-pos newgrid (:pos prior-snapshot))
		       nil)))))))

(defn update-grid-entity
  [gridstate oldentity newentity]
  (update-in gridstate [:grid] replace-entity oldentity newentity))

(defn forward-time
  [gridstate amount]
  (update-in gridstate [:time] + amount))

(defrecord GridState [grid time])

;; top-left is (0, 0); bottom-right is (width-1, height-1)

(defrecord Grid [width height gridvec])

(defn new-grid
  "Generate a width-by-height grid (represented as a 1-D array)
   full of nil entities."
  [width height]
  (Grid. width height (vec (repeat (* width height) nil))))

(defn entity-at
  "Get the entity at a posx, posy."
  [grid pos]
  (nth (:gridvec grid) (get-grid-pos grid pos)))

(defn pos-free?
  "Check if a position in the grid is free (not occupied by an entity)."
  [pos grid]
  (let [[x y] [(:x pos) (:y pos)]]
    (and (>= x 0) (< x (:width grid))
	 (>= y 0) (< y (:height grid))
	 (nil? (entity-at grid pos)))))

(defn symbol-used?
  "Check if a symbol has already been used by an existing entity."
  [symbol grid]
  (some #(= (:symbol %) symbol)	(:gridvec grid)))

(defn rand-symbol-and-pos
  "Generate a random symbol and position for a new entity."
  [grid]
  (let [rand-symbol #(char (+ 33 (rand-int 94)))
	rand-posx #(rand-int (:width grid))
	rand-posy #(rand-int (:height grid))]
    (loop [symbol (rand-symbol)
	   posx (rand-posx)
	   posy (rand-posy)]
      (if (or (symbol-used? symbol grid)
	      (not (pos-free? (Position. posx posy) grid)))
	(recur (rand-symbol) (rand-posx) (rand-posy))
	[symbol (Position. posx posy)]))))

(defn new-entity
  "Create a new entity with a random symbol and random (free) location."
  [grid]
  (let [[symbol pos] (rand-symbol-and-pos grid)]
    (Entity. symbol [(EntitySnapshot. pos)])))
  
(defn attempt-move
  "Try to move one step in a given direction; return new position."
  [dir pos grid]
  (let [left (Position. (dec (:x pos)) (:y pos))
	right (Position. (inc (:x pos)) (:y pos))
	down (Position. (:x pos) (inc (:y pos)))
	up (Position. (:x pos) (dec (:y pos)))]
    (case dir
	  "fixed" pos
	  "left" (if (pos-free? left grid) left pos)
	  "right" (if (pos-free? right grid) right pos)
	  "down" (if (pos-free? down grid) down pos)
	  "up" (if (pos-free? up grid) up pos))))

(defn walk1
  "Move an entity one step in a random (free) direction,
   and add that movement to the entity's history"
  [entity grid]
  (let [dir (nth ["left" "right" "down" "up" "fixed"] (rand-int 4))
	pos (attempt-move dir (pos entity) grid)]
    (add-snapshot entity (EntitySnapshot. pos))))

