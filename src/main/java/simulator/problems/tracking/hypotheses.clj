(ns simulator.problems.tracking.hypotheses
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.problems.tracking.events EventNew EventMove])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity add-event update-entity)])
  (:use [simulator.types.hypotheses :only (set-explainers)])
  (:use [simulator.strategies :only (add-log-msg)]))

(defn pair-near
  "For each spotted, find entities within walk distance."
  [spotted entities walk]
  (let [distfn (fn [s e] {:entity e :dist (manhattan-distance (pos s) (pos e))})]
    (for [s spotted]
      {:spotted s :entities
       (filter #(>= walk (:dist %))
	       (map (partial distfn s) entities))})))

(defn add-hyp
  [strat-state time hyp spotted]
  (let [hypspace (-> (:hypspace strat-state)
		     (update-in [:hyps] conj spotted)
		     (update-in [:hyps] conj hyp)
		     (set-explainers spotted #{hyp}))]
    (-> strat-state
	(update-in [:accepted] conj spotted)
	(update-in [:considering] conj hyp)
	(assoc :hypspace hypspace)
	(add-log-msg time
		     (if (= (:type hyp) "new")
		       (str "Hypothesizing that " spotted
			    " is new: " (:entity hyp))
		       (str "Hypothesizing that " spotted
			    " is the movement of " (:prev hyp)))))))

(defn add-hyp-new
  [strat-state spotted time]
  (let [entity (Entity. [(EntitySnapshot. time (pos spotted))])]
    (add-hyp strat-state time
	     {:type "new" :time time :spotted spotted :entity entity}
	     spotted)))

(defn add-hyp-move
  [strat-state spotted time prev]
  (let [event (EventMove. time (pos prev) (pos spotted))]
    (add-hyp strat-state time
	     {:type "move" :time time :prev prev :spotted spotted}
	     spotted)))

(defn filter-candidate-entities
  [time entities]
  (filter #(>= 2 (- time (:time (last (:snapshots %))))) entities))

(defn generate-hypotheses
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
			entity (Entity. [(EntitySnapshot. (:time hyp)
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

