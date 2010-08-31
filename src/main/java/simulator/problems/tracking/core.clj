(ns simulator.problems.tracking.core
  (:require [simulator.types problem])
  (:require [simulator.problems.tracking events eventlog grid entities])
  (:import [simulator.problems.tracking.eventlog EventLog])
  (:import [simulator.problems.tracking.events EventNew EventMove])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:import [simulator.types.problem Problem])
  (:import [simulator.problems.tracking.grid GridState])
  (:use clojure.set)
  (:use [simulator.evaluator :only (evaluate)])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos)])
  (:use [simulator.problems.tracking.eventlog :only
	 (add-entity add-event add-event-new add-event-move update-entity get-entities)])
  (:use [simulator.problems.tracking.grid :only
	 (new-grid new-entity update-grid-entity walk1 forward-time)])
  (:use [simulator.problems.tracking.sensors :only
	 (update-spotted generate-sensors-with-coverage measure-sensor-coverage)])
  (:use [simulator.strategies :only (init-strat-state explain)])
  (:use [simulator.types.hypotheses :only (set-explainers)]))

(def avg-fields [:Milliseconds :PercentCorrect
		 :StrategyCompute :StrategyMilliseconds :StrategyMemory
		 :Steps :NumberEntities :MaxWalk :AvgWalk
		 :ProbNewEntities :SensorReportNoise :BeliefNoise
		 :GridWidth :GridHeight :SensorCoverage :SensorOverlap])

(def non-avg-fields [:Strategy])

(def headers (concat avg-fields non-avg-fields))

(defn add-new-entities
  [trueevents gridstate numes]
  (loop [i 0
	 te trueevents
	 gs gridstate]
    (if (or (= i numes) ; stop if reached numes or if there's no more space in grid
	    (= (* (:width (:grid gs)) (:height (:grid gs)))
	       (count (get-entities te))))
      [te gs]
      (let [entity (new-entity (:grid gs) (:time gridstate))]
	(recur (inc i)
	       (-> te
		   (add-entity entity)
		   (add-event-new 0 (pos entity)))
	       (update-grid-entity gs nil entity))))))

(defn init-states
  [width height numes]
  (let [trueevents (EventLog. [] [])
	gridstate (GridState. (new-grid width height) 0)]
    (add-new-entities trueevents gridstate numes)))

(defn random-walks
  [walk trueevents gridstate]
  (let [time (:time gridstate) ;;; TODO: should time change for every move?
	entities (get-entities trueevents)
	entities-map (apply assoc {} (interleave entities entities))
	entity-walks (shuffle (apply concat (map #(repeat (rand-int (inc walk)) %)
						 entities)))]
    (loop [em entities-map
	   gs gridstate
	   ew entity-walks]
      (if (empty? ew)
	[(reduce (fn [te olde] (-> te
				 (update-entity time olde (pos (get em olde)))
				 (add-event-move time (pos olde) (pos (get em olde)))))
		 trueevents (keys em))
	 gs]
	(let [e (first ew)
	      olde (get em e)
	      newe (walk1 olde (:grid gs) time)]
	  (recur (assoc em e newe)
		 (update-grid-entity gs olde newe)
		 (rest ew)))))))

(defn possibly-add-new-entities
  [trueevents gridstate]
  (if (> 2.0 (rand)) [trueevents gridstate] ; skip adding new entities 95% of the time
      (add-new-entities trueevents gridstate (inc (rand-int 2)))))

(defn pair-near
  "For each spotted, find entities within walk distance."
  [spotted entities walk]
  (let [distfn (fn [s e] {:entity e :dist (manhattan-distance (pos s) (pos e))})]
    (for [s spotted]
      {:spotted s :entities
       (filter #(>= walk (:dist %))
	       (map (partial distfn s) entities))})))

(defn add-hyp
  [strat-state hyp spotted]
  (let [hypspace (-> (:hypspace strat-state)
		     (update-in [:hyps] conj spotted)
		     (update-in [:hyps] conj hyp)
		     (set-explainers spotted #{hyp}))
	ss (-> strat-state
	       (update-in [:accepted] conj spotted)
	       (update-in [:considering] conj hyp)
	       (assoc :hypspace hypspace))]
    ss))

(defn add-hyp-new
  [strat-state spotted time]
  (let [entity (Entity. \X [(EntitySnapshot. time (pos spotted))])]
    (add-hyp strat-state
	     {:type "new" :time time :spotted spotted :entity entity}
	     spotted)))

(defn add-hyp-move
  [strat-state spotted time prev]
  (let [event (EventMove. time (pos prev) (pos spotted))]
    (add-hyp strat-state
	     {:type "move" :time time :prev prev :spotted spotted}
	     spotted)))

(defn update-problem-data
  [strat-state]
  (loop [accepted (:accepted strat-state)
	 eventlog (:eventlog strat-state)]
    (if	(empty? accepted) (assoc strat-state :problem-data eventlog)
	(let [hyp (first accepted)]
	  (if (= (type hyp) simulator.problems.tracking.sensors.SensorEntity)
	    (recur (rest accepted) eventlog)
	    (case (:type hyp)
		  "new"
		  (let [event (EventNew. (:time hyp) (pos (:spotted hyp)))
			entity (Entity. \X [(EntitySnapshot. (:time hyp)
							     (pos (:spotted hyp)))])
			el (-> eventlog (add-event event) (add-entity entity))]
		    (recur (rest accepted) el))
		  "move"
		  (let [event (EventMove. (:time hyp) (pos (:prev hyp))
					  (pos (:spotted hyp)))
			el (-> eventlog (add-event event)
			       (update-entity (:time hyp) (:prev hyp)
					      (pos (:spotted hyp))))]
		    (recur (rest accepted) el))))))))

(defn filter-candidate-entities
  [time entities]
  (filter #(>= 2 (- time (:time (last (:snapshots %))))) entities))

(defn gen-nearest-hypotheses
  [strat-state sensors time]
  "The idea behind 'nearest' hypotheses is all spotted & existing entities will be
   paired according to k-closest pair (where k = number of spotted entities),
   and given these pairings, each spotted entity whose pairing has a distance
   less than some constant will be explained as having moved from its paired
   existing entity; all pairings with too great a distance will have 'new-entity'
   explanations."
  (let [unique-spotted (set (apply concat (map :spotted sensors)))
	candidate-entities (filter-candidate-entities time
			    (get-entities (:problem-data strat-state)))]
    
    (if (empty? candidate-entities)

      ;; no previously-known entities, hypothesize all as new
      (reduce (fn [ss spotted] (add-hyp-new ss spotted time))
	      strat-state unique-spotted)

      ;; got some previously-known entities, so associate them with spotted
      (loop [pairs (pair-near unique-spotted candidate-entities 10)
	     ss strat-state]
	
	(cond (empty? pairs) ss

	      ;; all entities too far; hypothesize new entity
	      (empty? (:entities (first pairs)))
	      (recur (rest pairs)
		     (add-hyp-new ss (:spotted (first pairs)) time))

	      ;; some entities in range; hypothesize them all
	      :else
	      (recur (rest pairs)
		     (reduce (fn [tempss e]
			       (add-hyp-move tempss (:spotted (first pairs)) time
					     (:entity e)))
			     ss (:entities (first pairs)))))))))

(defn single-step
  [params sensors [trueevents gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	ss (gen-nearest-hypotheses strat-state sens (:time gridstate))
	ss2 (update-problem-data (explain ss))
	[te gs] (random-walks (:MaxWalk params) trueevents (forward-time gridstate 1))
	[newte newgs] (possibly-add-new-entities te gs)]
    [newte newgs ss2]))

(defn last-explanation
  [sensors [trueevents gridstate strat-state]]
  (let [sens (map #(update-spotted % gridstate) sensors)
	ss (gen-nearest-hypotheses strat-state sens (:time gridstate))
	ss2 (update-problem-data (explain ss))]
    [trueevents ss2]))

(defn run
  [params strat-state]
  (let [sensors (generate-sensors-with-coverage
		  (:GridWidth params) (:GridHeight params) (:SensorCoverage params))
	[trueevents gridstate] (init-states
				(:GridWidth params) (:GridHeight params)
				(:NumberEntities params))
	startTime (. System (nanoTime))]
    (loop [i 0
	   combined-states [trueevents gridstate strat-state]]
      (if (< i (:Steps params))
	(recur (inc i) (single-step params sensors combined-states))
	(let [[te ss] (last-explanation sensors combined-states)]
	  {:trueevents te :stratstate ss :sensors sensors :results
	   (assoc params
	     :Milliseconds (/ (double (- (. System (nanoTime)) startTime)) 1000000.0)
	     :PercentCorrect (evaluate te ss)
	     :Strategy (:strategy ss)
	     :StrategyCompute 0
	     :StrategyMilliseconds 0
	     :StrategyMemory 0
	     :AvgWalk 0
	     :SensorCoverage (measure-sensor-coverage
			      (:GridWidth params) (:GridHeight params) sensors)
	     :SensorOverlap 0)})))))

(def tracking-problem
     (Problem. "tracking" run headers avg-fields non-avg-fields (EventLog. #{} #{})))

