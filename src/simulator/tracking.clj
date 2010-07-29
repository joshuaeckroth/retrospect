(ns simulator.tracking
  (:require [clojure.contrib.math :as math])
  (:require [clojure.set :as set])
  (:use incanter.core)
  (:use incanter.charts)
  (:use incanter.io)
  (:require [incanter.stats :as stats])
  (:import (java.io BufferedWriter FileWriter))
  (:import (java.awt Color Graphics Dimension))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JFrame)))

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
   (update-in this [:grid] replace-entity oldentity newentity))
  Temporal
  (forwardTime
   [this amount]
   (update-in this [:time] + amount)))

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
   (update-in this [:snapshots] conj snapshot))
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

(defrecord LogEntry [time msg])

(defprotocol StateMethods
  (addEntity [this entity])
  (updateEntity [this entity pos])
  (addEventNew [this time pos])
  (addEventMove [this time oldpos newpos])
  (addEvent [this event])
  (addLog [this time msg])
  (formatLogs [this]))

(defrecord State [events entities logs]
  StateMethods
  (addEntity
   [this entity]
   (update-in this [:entities] conj
	      (Entity. (if (:symbol entity) (:symbol entity) \X)
		       [(EntitySnapshot. (pos entity))])))
  (updateEntity
   ;;possibly use a reverse-lookup map in the future, to get entity keys
   ;;eg: (let [m {:a :b :c :d}] (zipmap (vals m) (keys m)))
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
   (update-in this [:events] conj event))
  (addLog
   [this time msg]
   (update-in this [:logs] conj (LogEntry. time msg)))
  (formatLogs
   [this]
   (apply str (map #(format "Time: %d, msg: %s\n" (:time %) (:msg %)) (:logs this)))))

(defn init-strat-state
  [strategy]
  (case strategy
	"guess" (State. [] [] [])
	"nearest" (State. [] [] [])))

(defn explain-new-entity
  [strat-state spotted time]
  (-> strat-state
      (addEntity spotted)
      (addEventNew time (pos spotted))))

(defn explain-existing-entity
  [strat-state spotted entity time]
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
		     (-> state
			 (addLog time (str "Guessing spotted " (toStr (first spotted)) " is new entity"))
			 (explain-new-entity (first spotted) time)))
	      :else
	      (recur (rest spotted)
		     (-> state
			 (addLog time (str "Guessing spotted " (toStr (first spotted))
					   " is continuation of " (toStr (nth es choice))))
			 (explain-existing-entity (first spotted) (nth es choice) time))))))))

(defn pair-nearest
  "This is an instance of the closest pairs problem. Note that, at the moment,
   the brute-force algorithm is used, which has complexity O(n^2)."
  [spotted entities]
  (let [pairs-of-pairs (for [s spotted]
			 (for [e entities]
			   {:spotted s :entity e :dist (manhattan-distance (pos s) (pos e))}))
	sorted-pairs (sort-by :dist (apply concat pairs-of-pairs))]
    (for [s spotted] (first (filter #(= (:spotted %) s) sorted-pairs)))))

;; 'new-entities' are always incorrect?
(defn explain-nearest
  "The idea behind 'explain-nearest' is all spotted & existing entities will be
   paired according to k-closest pair (where k = number of spotted entities),
   and given these pairings, each spotted entity whose pairing has a distance
   less than some constant will be explained as having moved from its paired
   existing entity; all pairings with too great a distance will have 'new-entity'
   explanations."
  [sensors state time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))]
    (if (empty? (:entities state))
      (reduce (fn [s spotted]
		(-> s
		    (addLog time (str "Explaining " (toStr spotted) " as new."))
		    (explain-new-entity spotted time)))
	      state unique-spotted)
      (loop [pairs (pair-nearest unique-spotted (:entities state))
	     s state]
	(cond (empty? pairs) s
	      (> (:dist (first pairs)) 5)
	      (recur (rest pairs)
		     (-> s
			 (addLog time (str "Explaining " (toStr (:spotted (first pairs)))
					   " as new since its distance to next-nearest is > 5"))
			 (explain-new-entity (:spotted (first pairs)) time)))
	      :else
	      (recur (rest pairs)
		     (-> s
			 (addLog time (str "Explaining " (toStr (:spotted (first pairs)))
					   " as continuation of next-nearest "
					   (toStr (:entity (first pairs)))))
			 (explain-existing-entity
			  (:spotted (first pairs))
			  (:entity (first pairs)) time))))))))

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
  (let [truestate (State. [] [] [])
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities truestate gridstate numes)))

(defn random-walks
  [walk truestate gridstate]
  (let [time (:time gridstate) ;;; TODO: should time change for every move?
	entities (:entities truestate)
	entities-map (apply assoc {} (interleave entities entities))
	entity-walks (shuffle (apply concat (map #(repeat (rand-int (inc walk)) %) entities)))]
    (loop [em entities-map
	   ts truestate
	   gs gridstate
	   ew entity-walks]
      (if (empty? ew) [ts gs]
	  (let [e (first ew)
		olde (get em e)
		newe (walk1 olde (:grid gs))]
	    (recur (assoc em e newe)
		   (-> ts
		       (updateEntity olde (pos newe))
		       (addEventMove time (pos olde) (pos newe)))
		   (updateGridEntity gs olde newe)
		   (rest ew)))))))

(defn possibly-add-new-entities
  [truestate gridstate]
  (if (> 0.95 (rand)) [truestate gridstate] ; skip adding new entities 95% of the time
      (add-new-entities truestate gridstate (inc (rand-int 2)))))

(defn single-step
  [walk strategy sensors [truestate gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	strat-s (explain strategy sens strat-state (:time gridstate))
	[ts gs] (random-walks walk truestate (forwardTime gridstate 1))
	[newts newgs] (possibly-add-new-entities ts gs)]
    [newts newgs strat-s]))

(defn last-explanation
  [strategy sensors [truestate gridstate strat-state]]
  [truestate gridstate
   (explain strategy (map #(update-spotted % gridstate) sensors)
	    strat-state (:time gridstate))])

(def *strategies* ["guess" "nearest"])

(def *truestate* nil)
(def *gridstate* nil)
     
(defn generate-results
  [msecs steps numes walk width height strategy sensor-coverage truestate gridstate strat-state]
  (print (formatLogs strat-state))
  (def *truestate* truestate)
  (def *gridstate* gridstate)
  (let [correct (evaluate truestate strat-state)
	incorrect (- (count (:events strat-state)) correct)
	total (count (:events truestate))
	percent (double (* 100 (/ correct total)))]
    [msecs steps numes walk width height strategy sensor-coverage
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

(defprotocol ResultsOperations
  (addResult [this result])
  (getResults [this]))

(defrecord Results [r]
  ResultsOperations
  (addResult [this result] (update-in this [:r] conj result))
  (getResults [this] (:r this)))

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
	numes [3]
	walk [1]
	width [10]
	height [10]
	strategy *strategies*
	sensor-coverage [100]
	sensors [(generate-sensors-with-coverage width height sensor-coverage)]]
    [steps numes walk width height strategy sensor-coverage sensors]))

(defn multiple-runs []
  (let [params (generate-run-params)
	results (parallel-runs params)]
    (reduce (fn [m r] (addResult m r)) (Results. []) results)))

(def *headers*
     ["Milliseconds" "Steps" "NumberEntities" "WalkSize"
      "GridWidth" "GridHeight" "Strategy" "SensorCoverage"
      "Correct" "Incorrect" "Total" "PercentCorrect"])

(defn write-csv
  [filename data]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (doseq [row data] (.write writer (apply str (concat (interpose "," row) [\newline]))))))

(defn save-results
  [results]
  (write-csv "results.csv" (concat [*headers*] (getResults results))))

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
		   (scatter-plot x y :x-label (name x) :y-label (name y) :legend true
				 :data ($where {:Strategy "guess"
						:SensorCoverage sensor-coverage})
				 :series-label "guess"
				 :title (format "Sensor coverage: %.0f%%" sensor-coverage))
		 (set-y-range 0.0 100.0)
		 (clear-background))]
      (if (nil? (rest *strategies*)) (view plot)
	  (view (reduce (fn [plot strategy]
			  (add-points plot x y
				      :data ($where {:Strategy strategy
						     :SensorCoverage sensor-coverage})
				      :series-label strategy))
			plot (rest *strategies*)))))))

(def *width* 10)
(def *height* 10)
(def *grid-cell-size* 50)
(def *animation-sleep-ms* 500)
(def *running* true)
(def *time* 0)
(def *event-log*
     (let [_ (apply run (first (generate-run-params)))]
       (:events *truestate*)))

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x *grid-cell-size*) (* y *grid-cell-size*) *grid-cell-size* *grid-cell-size*)))

(defn render-event [g e]
  (when (= (type e) simulator.tracking.EventNew)
    (fill-cell g (:x (:pos e)) (:y (:pos e)) (new Color 0 200 0 100)))
  (when (= (type e) simulator.tracking.EventMove)
    (fill-cell g (:x (:oldpos e)) (:y (:oldpos e)) (new Color 0 255 0 100))
    (fill-cell g (:x (:newpos e)) (:y (:newpos e)) (new Color 0 200 0 100))))

(defn render [g event-log cell-size width height time]
  (let [img (new BufferedImage
		 (* cell-size width)
		 (* cell-size height)
		 (. BufferedImage TYPE_INT_ARGB))
	bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun
     (for [e (filter #(= (:time %) time) event-log)]
       (render-event bg e)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn make-panel
  [event-log cell-size width height time]
     (doto (proxy [JPanel] []
	     (paint [g] (render g event-og cell-size width height time)))
       (.setPreferredSize
	(new Dimension
	     (* cell-size width)
	     (* cell-size height)))))

(defn make-frame
  [event-log cell-size width height time]
  (doto (new JFrame) (.add (make-pannel event-log cell-size width height time)) .pack .show))

(def animator (agent nil))

(defn animation [_]
  (when *running*
    (send-off *agent* #'animation))
  (. panel* (repaint))
  (. Thread (sleep *animation-sleep-ms*))
  (def *time* (inc *time*))
  (if (> *time* 50) (def *running* false))
  nil)

(comment
  (senf-off animator animation))