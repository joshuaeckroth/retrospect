(ns simulator.tracking
  (:require [clojure.contrib.math :as math])
  (:use artifice.org.clojure.utils))

(defprotocol EntityContainer
  (updateEntity [this entity]))

(defn replace-entity
  "Replace an entity in the grid (which may be nil) with a new one."
  [grid entity]
  (assoc grid :gridvec
	 (replace [(+ (* (:posy (first (:events entity))) (:width grid))
		      (:posx (first (:events entity)))) entity]
		  (:gridvec grid))))

(defrecord GridState [grid time]
  EntityContainer
  (updateEntity [this entity]
		(assoc this :grid (replace-entity (:grid this) entity))))

;; grid functions

;; top-left is (0, 0); bottom-right is (width-1, height-1)

(defrecord Grid [width height gridvec])

(defn new-grid
  "Generate a width-by-height grid (represented as a 1-D array)
   full of nil entities."
  [width height]
  (Grid. width height (vec (repeat (* width height) nil))))

(defn entity-at
  "Get the entity at a posx, posy."
  [grid posx posy]
  (nth (:gridvec grid) (+ (* posy (:width grid)) posx)))

(defn pos-free?
  "Check if a position in the grid is free (not occupied by an entity)."
  [posx posy grid]
  (and (>= posx 0) (< posx (:width grid))
       (>= posy 0) (< posy (:height grid))
       (nil? (entity-at grid posx posy))))

;; entity functions

(defprotocol EntityMethods
  (addEvent [this event]))

(defrecord EntityEvent [posx posy])

(defrecord Entity [symbol events]
  EntityMethods
  (addEvent [this event] (assoc this :events (conj (:events this) event))))

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
	      (not (pos-free? posx posy grid)))
	(recur (rand-symbol) (rand-posx) (rand-posy))
	[symbol posx posy]))))
  
(defn new-entity
  "Create a new entity with a random symbol and random (free) location."
  [grid]
  (let [[symbol posx posy] (rand-symbol-and-pos grid)]
    (Entity. symbol [(EntityEvent. posx posy)])))

(defn attempt-move
  "Try to move one step in a given direction; return new position."
  [dir posx posy grid]
  (switch dir
	  "left" (if (pos-free? (dec posx) posy grid)
		   [(dec posx) posy] [posx posy])
	  "right" (if (pos-free? (inc posx) posy grid)
		    [(inc posx) posy] [posx posy])
	  "down" (if (pos-free? posx (inc posy) grid)
		   [posx (inc posy)] [posx posy])
	  "up" (if (pos-free? posx (dec posy) grid)
		 [posx (dec posy)] [posx posy])))

(defn walk1
  "Move an entity one step in a random (free) direction,
   and add that movement to the entity's history"
  [entity grid]
  (let [dir (nth ["left" "right" "down" "up"] (rand-int 4))
	[posx posy] (attempt-move dir (:posx entity) (:posy entity) grid)]
    (addEvent entity (EntityEvent. posx posy))))

(defn walkn
  "Move an entity n steps in a random walk."
  [n entity grid]
  (loop [i 0
	 e entity]
    (if (< i n) (recur (inc i) (walk1 entity grid))
	e)))


;; sensor functions

(defrecord SensorEntity [time posx posy])

(defrecord Sensor [id left right bottom top spotted])

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (Sensor. id left right bottom top []))

(defn update-spotted
  "Create 'spotted' vector based on grid."
  [sensor gridstate]
  (assoc sensor :spotted
	 (map #(SensorEntity. (:time gridstate) (:posx %) (:posy %))
	      (filter #(not (nil? %))
		      (for [posx (range (:left sensor) (inc (:right sensor)))
			    posy (range (:bottom sensor) (inc (:top sensor)))]
			(entity-at (:grid gridstate) posx posy))))))

;; strategy functions

(defn init-strat-state
  [strategy])

(defn explain
  [strategy sensors strat-state gridstate]
  strat-state)

;; evaluation functions

(defn evaluate-explanation
  [old-strat-state new-strat-state]
  [1 0])

;; simulation functions

(defrecord Result [steps width height numes numsens
		   senscoverage sensoverlap strategy
		   decisions correct percent])

(defn init-grid-state
  [width height numes]
  (loop [i 0
	 gridstate (GridState. (new-grid width height) 0)]
    (if (< i numes)
      (recur (inc i)
	     (updateEntity gridstate (new-entity (:grid gridstate))))
      gridstate)))

(defn random-walks
  [walk gridstate]
  (reduce gridstate #(updateEntity gridstate (walkn walk % (:grid gridstate)))
	  (filter #(not (nil? %)) (:gridvec (:grid gridstate)))))

(defn single-step
  [decisions correct [walk strategy sensors strat-state gridstate]]
  (let [gs (random-walks walk gridstate)
	sens (map #(update-spotted % (:grid gs)) sensors)
	strat-s (explain strategy sens strat-state gs)
	[dec cor] (evaluate-explanation strat-state strat-s)]
    [(+ decisions dec) (+ correct cor) [walk strategy sens strat-s gs]]))

(defn run
  [steps numes walk width height strategy sensors]
  (let [[decisions correct]
	(loop [i 0
	       [decisions correct combined-states]
	       (single-step 0 0 [walk strategy sensors
				 (init-strat-state strategy)
				 (init-grid-state width height numes)])]
	  (if (< i steps)
	    (recur (inc i) (single-step decisions correct combined-states))
	    [decisions correct]))]
    (Result. steps width height numes (count sensors) 0 0
	     strategy decisions correct (float (/ correct decisions)))))