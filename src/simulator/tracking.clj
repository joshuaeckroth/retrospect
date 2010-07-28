(ns simulator.tracking
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set])
  (:use incanter.core)
  (:use incanter.charts)
  (:use incanter.io)
  (:require [incanter.stats :as stats]))

(defprotocol PositionMethods
  (equal [this other])
  (manhattan-distance [this other]))

(defrecord Position [x y]
  PositionMethods
  (equal [this other] (and (= (:x this) (:x other)) (= (:y this) (:y other))))
  (manhattan-distance
   [this other]
   (+ (math/abs (- (:x this) (:x other)))
      (math/abs (- (:y this) (:y other))))))

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
  [grid pos]
  (nth (:gridvec grid) (get-grid-pos grid pos)))

(defn pos-free?
  "Check if a position in the grid is free (not occupied by an entity)."
  [pos grid]
  (let [[x y] [(:x pos) (:y pos)]]
    (and (>= x 0) (< x (:width grid))
	 (>= y 0) (< y (:height grid))
	 (nil? (entity-at grid pos)))))

;; entity functions

(defprotocol EntityMethods
  (pos [this])
  (toStr [this]))

(defn print-entities
  [entities]
  (dorun (map #(println (toStr %)) entities)))

(defprotocol SnapshotMethods
  (addSnapshot [this snapshot]))

(defrecord EntitySnapshot [pos])

(defrecord Entity [symbol snapshots]
  SnapshotMethods
  (addSnapshot
   [this snapshot]
   (assoc this :snapshots (conj (:snapshots this) snapshot)))
  EntityMethods
  (pos [this] (:pos (last (:snapshots this))))
  (toStr [this] (format "Entity %c %s" (:symbol this)
			(apply str (interpose "->" (map #(format "(%d,%d)" (:x (:pos %)) (:y (:pos %)))
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
    (addSnapshot entity (EntitySnapshot. pos))))


;; sensor functions

(defrecord SensorEntity [time pos]
  EntityMethods
  (pos [this] (:pos this))
  (toStr [this] (format "SensorEntity (%d,%d)@%d" (:x (:pos this)) (:y (:pos this)) (:time this))))

(defrecord Sensor [id left right bottom top spotted])

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (Sensor. id left right bottom top []))

(defn update-spotted
  "Create 'spotted' vector based on grid."
  [sensor gridstate]
  (assoc sensor :spotted
	 (map #(SensorEntity. (:time gridstate) (pos %))
	      (filter #(not (nil? %))
		      (for [x (range (:left sensor) (inc (:right sensor)))
			    y (range (:bottom sensor) (inc (:top sensor)))]
			(entity-at (:grid gridstate) (Position. x y)))))))


;; generic strategy functions

(defrecord EventNew [time pos])

(defrecord EventMove [time oldpos newpos])

(defprotocol StratStateMethods
  (addEntity [this entity])
  (updateEntity [this entity pos])
  (addEventNew [this time pos])
  (addEventMove [this time oldpos newpos])
  (addEvent [this event]))

(defrecord StratState [events entities]
  StratStateMethods
  (addEntity
   [this entity]
   (assoc this :entities
	  (conj (:entities this)
		(Entity. \X [(EntitySnapshot. (pos entity))]))))
  (updateEntity
					;possibly use a reverse-lookup map in the future, to get entity keys
					;eg: (let [m {:a :b :c :d}] (zipmap (vals m) (keys m)))
   [this entity pos]
   (assoc this :entities
	  (map #(if (not= % entity) %
		    (addSnapshot % (EntitySnapshot. pos)))
	       (:entities this))))
  (addEventNew
   [this time pos]
   (addEvent this (EventNew. time pos)))
  (addEventMove
   [this time oldpos newpos]
   (addEvent this (EventMove. time oldpos newpos)))
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
      (addEventNew time (pos spotted))))

(defn explain-existing-entity
  [spotted entity time strat-state]
  (-> strat-state
      (updateEntity entity (pos spotted))
      (addEventMove time (pos entity) (pos spotted))))

(defn explain-guess
  [sensors strat-state time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))
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

(defn pair-nearest
  "This is an instance of the closest pairs problem. Note that, at the moment,
   the brute-force algorithm is used, which has complexity O(n^2)."
  [spotted entities]
  (let [pairs-of-pairs (for [s spotted]
			 (for [e entities]
			   {:spotted s :entity e :dist (manhattan-distance (pos s) (pos e))}))
	sorted-pairs (sort-by :dist (apply concat pairs-of-pairs))]
    (for [s spotted] (first (filter #(= (:spotted %) s) sorted-pairs)))))

					; 'new-entities' are always incorrect?
(defn explain-nearest
  "The idea behind 'explain-nearest' is all spotted & existing entities will be
   paired according to k-closest pair (where k = number of spotted entities),
   and given these pairings, each spotted entity whose pairing has a distance
   less than some constant will be explained as having moved from its paired
   existing entity; all pairings with too great a distance will have 'new-entity'
   explanations."
  [sensors strat-state time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))]
    (if (empty? (:entities strat-state))
      (reduce (fn [state spotted] (explain-new-entity spotted time state))
	      strat-state unique-spotted)
      (loop [pairs (pair-nearest unique-spotted (:entities strat-state))
	     state strat-state]
	(cond (empty? pairs) state
	      (> (:dist (first pairs)) 5)
	      (recur (rest pairs) (explain-new-entity (:spotted (first pairs)) time state))
	      :else
	      (recur (rest pairs)
		     (explain-existing-entity (:spotted (first pairs))
					      (:entity (first pairs)) time state)))))))

(defn explain
  [strategy sensors strat-state time]
  (case strategy
	"guess" (explain-guess sensors strat-state time)
	"nearest" (explain-nearest sensors strat-state time)))

;; evaluation functions

(defn evaluate
  [truestate strat-state]
  (count (set/intersection (set (:events truestate)) (set (:events strat-state)))))

;; simulation functions

(defn add-new-entities
  [truestate gridstate numes]
  (loop [i 0
	 ts truestate
	 gs gridstate]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width (:grid gs)) (:height (:grid gs)))
	       (count (:entities ts))))
      [ts gs]
      (let [entity (new-entity (:grid gs))]
	(recur (inc i)
	       (-> ts
		   (addEntity entity)
		   (addEventNew 0 (pos entity)))
	       (updateGridEntity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [truestate (StratState. [] [])
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities truestate gridstate numes)))

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
    (if (= i (count entities)) ts
	(let [olde (nth entities i)
	      newe (nth newes i)]
	  (recur (inc i)
		 (-> ts
		     (updateEntity olde (pos newe))
		     (addEventMove time (pos olde) (pos newe))))))))

(defn random-walks-n
  [walk truestate gridstate]
  (let [entities (shuffle (:entities truestate))]
    (loop [i 0
	   es entities
	   gs gridstate]
      (let [[newes newgs] (random-walks es gs)]
	(if (= i walk) [(update-truestate truestate entities newes (:time newgs)) newgs]
	    (recur (inc i) newes newgs))))))

(defn possibly-add-new-entities
  [truestate gridstate]
  (if (> 0.95 (rand)) [truestate gridstate] ; skip adding new entities 95% of the time
      (add-new-entities truestate gridstate (inc (rand-int 2)))))

(defn single-step
  [walk strategy sensors [truestate gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	strat-s (explain strategy sens strat-state (:time gridstate))
	[ts gs] (random-walks-n walk truestate (forwardTime gridstate 1))
	[newts newgs] (possibly-add-new-entities ts gs)]
    [newts newgs strat-s]))

(defn last-explanation
  [strategy sensors [truestate gridstate strat-state]]
  [truestate gridstate
   (explain strategy (map #(update-spotted % gridstate) sensors)
	    strat-state (:time gridstate))])

(def *strategies* ["guess" "nearest"])

(defn get-strategy-index
  [strategy]
  (loop [i 0]
    (if (= strategy (nth *strategies* i)) i
	(recur (inc i)))))
     
(defn generate-results
  [msecs steps numes walk width height strategy sensor-coverage truestate gridstate strat-state]
  (let [correct (evaluate truestate strat-state)
	incorrect (- (count (:events strat-state)) correct)
	total (count (:events truestate))
	percent (* 100 (/ correct total))
	strategy-index (get-strategy-index strategy)]
    [msecs steps numes walk width height strategy-index sensor-coverage
     correct incorrect total percent]))

(defn run
  [steps numes walk width height strategy sensor-coverage sensors]
  (let [[truestate gridstate] (init-states width height numes)
	strat-state (init-strat-state strategy)
	startTime (. System (nanoTime))]
    (loop [i 0
	   combined-states [truestate gridstate strat-state]]
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
  (apply concat (pmap (fn [partition] (map #(apply run %) partition))
		      (partition-all 100 (shuffle params)))))

(defn generate-run-params []
  (for [steps [50]
	numes [1 2 5 10 20 30]
	walk [1 2 3 5 7]
	width [10]
	height [10]
	strategy *strategies*
	sensor-coverage [10 50 75 100]
	sensors [(generate-sensors-with-coverage width height sensor-coverage)]]
    [steps numes walk width height strategy sensor-coverage sensors]))

(defn multiple-runs []
  (let [params (generate-run-params)
	results (parallel-runs params)]
    (reduce (fn [m r] (addResult m r)) (ResultsMatrix. []) results)))

(def *headers*
     {:Milliseconds "Milliseconds"
      :Steps "Steps"
      :Number-entities "Number-entities"
      :Walk-size "Walk-size"
      :Grid-width "Grid-width"
      :Grid-height "Grid height"
      :Strategy-index "Strategy-index"
      :Sensor-coverage "Sensor-coverage"
      :Correct "Correct"
      :Incorrect "Incorrect"
      :Total "Total"
      :Percent-correct "Percent-correct"})

(defn save-results
  [matrix]
  (save (getMatrix matrix) "results.csv" :header ["Milliseconds"
						  "Steps"
						  "Number-entities"
						  "Walk-size"
						  "Grid-width"
						  "Grid-height"
						  "Strategy-index"
						  "Sensor-coverage"
						  "Correct"
						  "Incorrect"
						  "Total"
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

(defn plot
  [data x y sensor-coverage]
  (with-data data
    (let [plot (doto
		   (scatter-plot x y :x-label (*headers* x) :y-label (*headers* y) :legend true
				 :data ($where {:Strategy-index 0
						:Sensor-coverage sensor-coverage})
				 :series-label "guess"
				 :title (format "Sensor coverage: %.0f%%" sensor-coverage))
		 (set-y-range 0.0 100.0)
		 (clear-background))]
      (if (nil? (rest *strategies*)) (view plot)
	  (view (reduce (fn [plot strategy]
			  (add-points plot x y
				      :data ($where {:Strategy-index (get-strategy-index strategy)
						     :Sensor-coverage sensor-coverage})
				      :series-label strategy))
			plot (rest *strategies*)))))))