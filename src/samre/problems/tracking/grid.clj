(ns samre.problems.tracking.grid
  (:require [samre.problems.tracking positions entities])
  (:import [samre.problems.tracking.positions Position])
  (:import [samre.problems.tracking.entities Entity EntitySnapshot])
  (:use [samre.problems.tracking.entities :only (pos add-snapshot)]))

;; top-left is (0, 0); bottom-right is (width-1, height-1)

(defrecord Grid [width height gridvec])

(defn get-grid-pos
  [grid pos]
  (+ (* (:y pos) (:width grid)) (:x pos)))

(defn replace-grid-entity
  "Replace an entity in the grid (which may be nil) with a new one."
  [grid oldentity newentity]
  (let [snapshot (last (:snapshots newentity))
	newgrid (assoc grid :gridvec
		       (assoc (:gridvec grid)
			 (get-grid-pos grid (:pos snapshot)) newentity))]
    (if (nil? oldentity) newgrid
	(let [prior-snapshot (last (:snapshots oldentity))]
	  (if (= (:pos prior-snapshot) (:pos snapshot)) newgrid
	      (assoc newgrid :gridvec
		     (assoc (:gridvec newgrid)
		       (get-grid-pos newgrid (:pos prior-snapshot))
		       nil)))))))

(defn new-grid
  "Generate a width-by-height grid (represented as a 1-D array)
   full of nil entities."
  [params]
  (Grid. (:GridWidth params) (:GridHeight params)
         (vec (repeat (* (:GridWidth params) (:GridHeight params)) nil))))

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

(defn rand-pos
  "Generate a random position for a new entity."
  [grid]
  (let [rand-posx #(rand-int (:width grid))
	rand-posy #(rand-int (:height grid))]
    (loop [posx (rand-posx)
	   posy (rand-posy)]
      (if (not (pos-free? (Position. posx posy) grid))
	(recur (rand-posx) (rand-posy))
	(Position. posx posy)))))

(defn new-entity
  "Create a new entity with a random (free) location."
  [grid time]
  (let [pos (rand-pos grid)]
    (Entity. [(EntitySnapshot. time pos)])))
  
(defn attempt-move
  "Try to move one step in a given direction; return new position."
  [dir pos grid]
  (let [left (Position. (dec (:x pos)) (:y pos))
	right (Position. (inc (:x pos)) (:y pos))
	down (Position. (:x pos) (inc (:y pos)))
	up (Position. (:x pos) (dec (:y pos)))]
    (case dir
	  "left" (if (pos-free? left grid) left)
	  "right" (if (pos-free? right grid) right)
	  "down" (if (pos-free? down grid) down)
	  "up" (if (pos-free? up grid) up))))

(defn walk1
  "Move an entity one step in a random (free) direction,
   and add that movement to the entity's history; try to
   move 4 times, but give up if no move comes out by that point."
  [entity grid time]
  (loop [attempts 0]
    (if (< attempts 4)
      (let [dir (nth ["left" "right" "down" "up"] (rand-int 4))
            pos (attempt-move dir (pos entity) grid)]
        (if pos
          (add-snapshot entity (EntitySnapshot. time pos))
          (recur (inc attempts)))))))

