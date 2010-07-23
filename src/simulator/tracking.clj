(ns simulator.tracking
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set])
  (:use incanter.core)
  (:use incanter.charts)
  (:use incanter.io)
  (:require [incanter.stats :as stats]))

(defn get-grid-pos
  [grid posx posy]
  (+ (* posy (:width grid)) posx))

(defn replace-entity
  "Replace an entity in the grid (which may be nil) with a new one."
  [grid oldentity newentity]
  (let [snapshot (last (:snapshots newentity))
	newgrid (assoc grid :gridvec
		       (assoc (:gridvec grid)
			 (get-grid-pos grid (:posx snapshot) (:posy snapshot)) newentity))]
    (if (nil? oldentity) newgrid
	(let [prior-snapshot (last (:snapshots oldentity))]
	  (if (or (not= (:posx prior-snapshot) (:posx snapshot))
		  (not= (:posy prior-snapshot) (:posy snapshot)))
	    (assoc newgrid :gridvec (assoc (:gridvec newgrid)
				      (get-grid-pos newgrid (:posx prior-snapshot)
						    (:posy prior-snapshot)) nil))
	    newgrid)))))

(defprotocol EntityContainer
  (updateGridEntity [this oldentity newentity]))

(defprotocol Temporal
  (forwardTime [this amount]))

(defrecord GridState [grid time]
  EntityContainer
  (updateGridEntity
   [this oldentity newentity]
   (assoc this :grid (replace-entity (:grid this) oldentity newentity)))
  Temporal
  (forwardTime
   [this amount]
   (assoc this :time (+ amount (:time this)))))

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
  (nth (:gridvec grid) (get-grid-pos grid posx posy)))

(defn pos-free?
  "Check if a position in the grid is free (not occupied by an entity)."
  [posx posy grid]
  (and (>= posx 0) (< posx (:width grid))
       (>= posy 0) (< posy (:height grid))
       (nil? (entity-at grid posx posy))))

;; entity functions

(defprotocol EntityMethods
  (lastPosX [this])
  (lastPosY [this])
  (toStr [this]))

(defn print-entities
  [entities]
  (dorun (map #(println (toStr %)) entities)))

(defprotocol SnapshotMethods
  (addSnapshot [this snapshot]))

(defrecord EntitySnapshot [posx posy])

(defrecord Entity [symbol snapshots]
  SnapshotMethods
  (addSnapshot
   [this snapshot]
   (assoc this :snapshots (conj (:snapshots this) snapshot)))
  EntityMethods
  (lastPosX [this] (:posx (last (:snapshots this))))
  (lastPosY [this] (:posy (last (:snapshots this))))
  (toStr [this] (format "Entity %c %s" (:symbol this)
			(apply str (interpose "->" (map #(format "(%d,%d)" (:posx %) (:posy %))
							(:snapshots this)))))))
  
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
    (Entity. symbol [(EntitySnapshot. posx posy)])))

(defn attempt-move
  "Try to move one step in a given direction; return new position."
  [dir posx posy grid]
  (case dir
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
	[posx posy] (attempt-move dir (lastPosX entity) (lastPosY entity) grid)]
    (addSnapshot entity (EntitySnapshot. posx posy))))


;; sensor functions

(defrecord SensorEntity [time posx posy]
  EntityMethods
  (lastPosX [this] (:posx this))
  (lastPosY [this] (:posy this))
  (toStr [this] (format "SensorEntity (%d,%d)@%d" (:posx this) (:posy this) (:time this))))

(defrecord Sensor [id left right bottom top spotted])

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (Sensor. id left right bottom top []))

(defn update-spotted
  "Create 'spotted' vector based on grid."
  [sensor gridstate]
  (assoc sensor :spotted
	 (map #(SensorEntity. (:time gridstate) (lastPosX %) (lastPosY %))
	      (filter #(not (nil? %))
		      (for [posx (range (:left sensor) (inc (:right sensor)))
			    posy (range (:bottom sensor) (inc (:top sensor)))]
			(entity-at (:grid gridstate) posx posy))))))


;; generic strategy functions

(defrecord EventNew [time posx posy])

(defrecord EventMove [time oposx oposy posx posy])

(defprotocol StratStateMethods
  (addEntity [this entity])
  (updateEntity [this entity posx posy])
  (addEventNew [this time posx posy])
  (addEventMove [this time oposx oposy posx posy])
  (addEvent [this event]))

(defrecord StratState [events entities]
  StratStateMethods
  (addEntity
   [this entity]
   (assoc this :entities
	  (conj (:entities this)
		(Entity. \X [(EntitySnapshot. (lastPosX entity) (lastPosY entity))]))))
  (updateEntity
					;possibly use a reverse-lookup map in the future, to get entity keys
					;eg: (let [m {:a :b :c :d}] (zipmap (vals m) (keys m)))
   [this entity posx posy]
   (assoc this :entities
	  (map #(if (= % entity)
		  (addSnapshot % (EntitySnapshot. posx posy))
		  %) (:entities this))))
  (addEventNew
   [this time posx posy]
   (addEvent this (EventNew. time posx posy)))
  (addEventMove
   [this time oposx oposy posx posy]
   (addEvent this (EventMove. time oposx oposy posx posy)))
  (addEvent
   [this event]
   (assoc this :events (conj (:events this) event))))

(defn init-strat-state
  [strategy]
  (case strategy
	"guess" (StratState. [] [])
	"nearest" (StratState. [] [])))

(defn explain-new-entity
  [spotted time strat-state]
  (-> strat-state
      (addEntity spotted)
      (addEventNew time (lastPosX spotted) (lastPosY spotted))))

(defn explain-existing-entity
  [spotted entity time strat-state]
  (-> strat-state
      (updateEntity entity (lastPosX spotted) (lastPosY spotted))
      (addEventMove time (lastPosX entity) (lastPosY entity)
		    (lastPosX spotted) (lastPosY spotted))))

(defn explain-guess
  [sensors strat-state gridstate]
  (let [time (:time gridstate)
	unique-spotted (set (apply concat (map :spotted sensors)))
	es (:entities strat-state)
	numes (count es)]
    (loop [spotted unique-spotted
	   state strat-state]
      (let [choice (rand-int (inc numes))]
	(cond (empty? spotted) state
	      (= choice numes)
	      (recur (rest spotted)
		     (explain-new-entity
		      (first spotted) time state))
	      :else
	      (recur (rest spotted)
		     (explain-existing-entity
		      (first spotted) (nth es choice) time state)))))))

(defn manhattan-distance
  [e1 e2]
  (+ (math/abs (- (lastPosX e1) (lastPosX e2)))
     (math/abs (- (lastPosY e1) (lastPosY e2)))))

(defn pair-nearest
  "This is an instance of the closest pairs problem. Note that, at the moment,
   the brute-force algorithm is used, which has complexity O(n^2)."
  [spotted entities]
  (let [pairs
	(sort-by :dist 
		 (apply concat
			(for [s spotted]
			  (for [e entities]
			    {:spotted s :entity e :dist (manhattan-distance s e)}))))]
    (for [s spotted]
      (first (filter #(= (:spotted %) s) pairs)))))

					; 'new-entities' are always incorrect?
(defn explain-nearest
  "The idea behind 'explain-nearest' is all spotted & existing entities will be
   paired according to k-closest pair (where k = number of spotted entities),
   and given these pairings, each spotted entity whose pairing has a distance
   less than some constant will be explained as having moved from its paired
   existing entity; all pairings with too great a distance will have 'new-entity'
   explanations."
  [sensors strat-state gridstate]
  (let [time (:time gridstate)
	unique-spotted (set (apply concat (map :spotted sensors)))]
    (if (empty? (:entities strat-state))
      (reduce (fn [state spotted] (explain-new-entity spotted time state))
	      strat-state unique-spotted)
      (loop [pairs (pair-nearest unique-spotted (:entities strat-state))
	     state strat-state]
	(cond (empty? pairs) state
	      (> (:dist (first pairs)) 5)
	      (recur (rest pairs) (explain-new-entity (:spotted (first pairs)) time state))
	      :else
	      (recur (rest pairs) (explain-existing-entity
				   (:spotted (first pairs))
				   (:entity (first pairs))
				   time state)))))))

(defn explain
  [strategy sensors strat-state gridstate]
  (case strategy
	"guess" (explain-guess sensors strat-state gridstate)
	"nearest" (explain-nearest sensors strat-state gridstate)))

;; evaluation functions

(defn evaluate
  [truestate strat-state]
  (count (set/intersection (set (:events truestate)) (set (:events strat-state)))))

;; simulation functions

(defn init-grid-state
  [width height numes]
  (loop [i 0
	 gridstate (GridState. (new-grid width height) 0)
	 entities []]
    (if (< i numes)
      (let [entity (new-entity (:grid gridstate))]
	(recur (inc i) (updateGridEntity gridstate nil entity) (conj entities entity)))
      [gridstate entities])))

(defn init-true-state
  [entities]
  (reduce (fn [ts entity] (addEventNew ts 0 (lastPosX entity) (lastPosY entity)))
	  (StratState. [] entities) entities))

(defn random-walks
  [entities gridstate]
  (loop [es entities
	 newes []
	 gs gridstate]
    (if (empty? es) [newes gs]
	(let [newe (walk1 (first es) (:grid gs))]
	  (recur (rest es) (conj newes newe)
		 (updateGridEntity gs (first es) newe))))))

(defn update-truestate
  [truestate entities newes time]
  (loop [i 0
	 ts truestate]
    (if (< i (count entities))
      (let [olde (nth entities i)
	    newe (nth newes i)]
	(recur (inc i) (-> ts
			   (updateEntity olde (lastPosX newe) (lastPosY newe))
			   (addEventMove time (lastPosX olde) (lastPosY olde)
					 (lastPosX newe) (lastPosY newe)))))
      ts)))

(defn random-walks-n
  [walk truestate gridstate]
  (let [entities (shuffle (:entities truestate))]
    (loop [i 0
	   es entities
	   gs gridstate]
      (let [[newes newgs] (random-walks es gs)]
	(if (= i walk) [(update-truestate truestate entities newes (:time newgs)) newgs]
	    (recur (inc i) newes newgs))))))

(defn single-step
  [walk strategy sensors [truestate strat-state gridstate]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	strat-s (explain strategy sens strat-state gridstate)
	[ts gs] (random-walks-n walk truestate (forwardTime gridstate 1))]
    [ts strat-s gs]))

(defn last-explanation
  [strategy sensors [truestate strat-state gridstate]]
  [truestate (explain strategy
		      (map #(update-spotted % gridstate) sensors)
		      strat-state gridstate)
   gridstate])

(def *strategies* ["guess" "nearest"])

(defn get-strategy-index
  [strategy]
  (loop [i 0]
    (if (= strategy (nth *strategies* i)) i
	(recur (inc i)))))
     
(defn generate-results
  [msecs steps numes walk width height strategy sensor-coverage truestate strat-state gridstate]
  (let [correct (evaluate truestate strat-state)
	incorrect (- (count (:events strat-state)) correct)
	total (count (:events truestate))
	percent (* 100 (/ correct total))
	strategy-index (get-strategy-index strategy)]
    [msecs steps numes walk width height strategy-index sensor-coverage
     correct incorrect total percent]))

(defn run
  [steps numes walk width height strategy sensor-coverage sensors]
  (let [[gridstate entities] (init-grid-state width height numes)
	startTime (. System (nanoTime))]
    (loop [i 0
	   combined-states [(init-true-state entities)
			    (init-strat-state strategy)
			    gridstate]]
      (if (< i steps)
	(recur (inc i) (single-step walk strategy sensors combined-states))
	(apply generate-results
	       (/ (double (- (. System (nanoTime)) startTime)) 1000000.0)
	       steps numes walk width height strategy sensor-coverage
	       (last-explanation strategy sensors combined-states))))))

(defprotocol ResultsMatrixOperations
  (addResult [this result])
  (getMatrix [this]))

(defrecord ResultsMatrix [m]
  ResultsMatrixOperations
  (addResult
   [this result]
   (assoc this :m (conj (:m this) result)))
  (getMatrix [this] (matrix (:m this))))

(defn generate-sensors-with-coverage
  [width height coverage]
  (let [area (* width height)
	to-cover (int (* (/ coverage 101) area)) ; use 101 in denom to ensure top < height
	left 0
	right (dec (if (< to-cover width) to-cover width))
	bottom 0
	top (int (/ to-cover width))]
    [(new-sensor "X" left right bottom top)]))

(defn parallel-runs
  [params]
  (apply concat (pmap (fn [partition]
			(time (doall (map #(apply run %) partition))))
		      (partition-all (/ (count params) 16) (shuffle params)))))

(defn generate-run-params []
  (for [steps [1 10]
	numes [1 10]
	walk [1 3 5]
	width [5 10 20]
	height [5 10 20]
	strategy ["guess" "nearest"]
	sensor-coverage [10 50 100]
	sensors [(generate-sensors-with-coverage width height sensor-coverage)]]
    [steps numes walk width height strategy sensor-coverage sensors]))

(defn multiple-runs []
  (let [params (generate-run-params)
	results (parallel-runs params)]
    (reduce (fn [m r] (addResult m r)) (ResultsMatrix. []) results)))

(defn save-results
  [matrix]
  (save (getMatrix matrix) "results.csv" :header ["Milliseconds" "Steps" "Number-entities" "Walk-size"
						  "Grid-width" "Grid-height" "Strategy-index"
						  "Sensor-coverage" "Correct" "Incorrect" "Total"
						  "Percent-correct"]))

(defn read-results []
  (read-dataset "results.csv" :header true))

(comment
  (save-results (multiple-runs))
  (with-data (read-results)
    (view (scatter-plot :Walk-size :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (view (scatter-plot :Number-entities :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (view (scatter-plot :Steps :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (view (box-plot :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (let [plot (scatter-plot :Steps :Milliseconds)]
      (view plot)
      (add-lines plot (sel (read-results) :cols 1)
		 (:fitted (stats/linear-model
			   (sel (read-results) :cols 0)
			   (sel (read-results) :cols 1))))))
  (with-data (sel (read-results) :filter #(= 50.0 (nth % 7)))
    (view (scatter-plot :Walk-size :Percent-correct :group-by :Strategy-index :legend true))))

