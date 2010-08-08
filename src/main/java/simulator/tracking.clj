(ns simulator.tracking
  (:require [simulator.types entities states results])
  (:require [simulator.tracking grid])
  (:import [simulator.types.entities Entity EntitySnapshot])
  (:import [simulator.types.states State])
  (:import [simulator.types.results Result])
  (:import [simulator.tracking.grid GridState])
  (:use [simulator.types.generic :only (to-str forward-time)])
  (:use [simulator.types.parameters :only (ParameterMethods)])
  (:use [simulator.types.entities :only (add-snapshot pos)])
  (:use [simulator.types.states :only
	 (add-entity add-event-new add-event-move update-entity get-entities get-events)])
  (:use [simulator.evaluator :only (evaluate)])
  (:use [simulator.strategies :only (init-strat-state)])
  (:use [simulator.strategies.explain :only (explain)])
  (:use [simulator.tracking.grid :only
	 (update-grid-entity new-grid get-grid-pos attempt-move rand-symbol-and-pos)])
  (:use [simulator.tracking.sensors :only
	 (update-spotted generate-sensors-with-coverage measure-sensor-coverage)]))

(defrecord TrackingParameters
  [headers steps numes walk width height strategy sensor-coverage]
  ParameterMethods
  (get-headers [this] headers)
  (get-params
   [this]
   (for [_steps steps
	 _numes numes
	 _walk walk
	 _width width
	 _height height
	 _strategy strategy
	 _sensor-coverage sensor-coverage
	 _sensors [(generate-sensors-with-coverage _width _height _sensor-coverage)]]
     [_steps _numes _walk _width _height _strategy _sensor-coverage _sensors]))
  (to-xml [this]
	 [:params
	  [:steps (str steps)]
	  [:numes (str numes)]
	  [:walk (str walk)]
	  [:width (str width)]
	  [:height (str height)]
	  [:strategy (str strategy)]
	  [:sensor-coverage (str sensor-coverage)]]))

(defn generate-params []
  (TrackingParameters.
   ["Steps" "NumberEntities" "WalkSize" "GridWidth" "GridHeight" "Strategy"]
   [50] [1 2 3 4 5 6 7 8 9 10 20 30 40 50 60 70 80 90]
   [1 2 3 4 5 6 7 8 9 10 15 20] [10] [10]
   ["guess" "nearest"]
   [0 10 20 30 40 50 60 70 80 90 100]))

(defn new-entity
  "Create a new entity with a random symbol and random (free) location."
  [grid]
  (let [[symbol pos] (rand-symbol-and-pos grid)]
    (Entity. symbol [(EntitySnapshot. pos)])))

(defn walk1
  "Move an entity one step in a random (free) direction,
   and add that movement to the entity's history"
  [entity grid]
  (let [dir (nth ["left" "right" "down" "up" "fixed"] (rand-int 4))
	pos (attempt-move dir (pos entity) grid)]
    (add-snapshot entity (EntitySnapshot. pos))))

(defn add-new-entities
  [truestate gridstate numes]
  (loop [i 0
	 ts truestate
	 gs gridstate]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width (:grid gs)) (:height (:grid gs)))
	       (count (get-entities ts))))
      [ts gs]
      (let [entity (new-entity (:grid gs))]
	(recur (inc i)
	       (-> ts
		   (add-entity entity)
		   (add-event-new 0 (pos entity)))
	       (update-grid-entity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [truestate (State. [] [])
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities truestate gridstate numes)))

(defn random-walks
  [walk truestate gridstate]
  (let [time (:time gridstate) ;;; TODO: should time change for every move?
	entities (get-entities truestate)
	entities-map (apply assoc {} (interleave entities entities))
	entity-walks (shuffle (apply concat (map #(repeat (rand-int (inc walk)) %) entities)))]
    (loop [em entities-map
	   gs gridstate
	   ew entity-walks]
      (if (empty? ew)
	[(reduce (fn [ts olde] (-> ts
				 (update-entity olde (pos (get em olde)))
				 (add-event-move time (pos olde) (pos (get em olde)))))
		 truestate (keys em))
	 gs]
	(let [e (first ew)
	      olde (get em e)
	      newe (walk1 olde (:grid gs))]
	  (recur (assoc em e newe)
		 (update-grid-entity gs olde newe)
		 (rest ew)))))))

(defn possibly-add-new-entities
  [truestate gridstate]
  (if (> 2.0 (rand)) [truestate gridstate] ; skip adding new entities 95% of the time
      (add-new-entities truestate gridstate (inc (rand-int 2)))))

(defn single-step
  [walk strategy sensors [truestate gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	strat-s (explain strat-state sens (:time gridstate))
	[ts gs] (random-walks walk truestate (forward-time gridstate 1))
	[newts newgs] (possibly-add-new-entities ts gs)]
    [newts newgs strat-s]))

(defn last-explanation
  [strategy sensors [truestate gridstate strat-state]]
  [truestate gridstate
   (explain strat-state (map #(update-spotted % gridstate) sensors)
	    (:time gridstate))])

(defn generate-results
  [msecs steps numes walk width height strategy sensors truestate gridstate strat-state]
  (let [correct (evaluate truestate strat-state)
	incorrect (- (count (get-events strat-state)) correct)
	total (count (get-events truestate))
	percent (double (* 100 (/ correct total)))]
    (Result. msecs percent (measure-sensor-coverage width height sensors) truestate strat-state
	     [steps numes walk width height strategy])))

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
	       steps numes walk width height strategy sensors
	       (last-explanation strategy sensors combined-states))))))
