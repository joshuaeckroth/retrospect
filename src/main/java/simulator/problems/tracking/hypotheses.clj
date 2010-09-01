(ns simulator.problems.tracking.hypotheses
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.problems.tracking.events EventNew EventMove])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity add-event update-entity)])
  (:use [simulator.types.hypotheses :only (add-explainers set-apriori)])
  (:use [simulator.strategies :only (add-log-msg)]))

(defrecord TrackingHyp [type time spotted entity prev]
  Object
  (toString [_] (format "TrackingHyp (%s)@%d (spotted: %s) %s"
			type time spotted
			(if (= type "new") entity prev))))

(defn pair-near
  "For each spotted, find entities within walk distance."
  [spotted entities walk]
  (let [distfn (fn [s e] {:entity e :dist (manhattan-distance (pos s) (pos e))})]
    (for [s spotted]
      {:spotted s :entities
       (filter #(>= walk (:dist %))
	       (map (partial distfn s) entities))})))

(defn add-hyp
  [strat-state time hyp spotted apriori]
  (let [hypspace (-> (:hypspace strat-state)
		     (update-in [:hyps] conj spotted)
		     (update-in [:hyps] conj hyp)
		     (add-explainers spotted #{hyp})
		     (set-apriori spotted 1.0)
		     (set-apriori hyp apriori))]
    (-> strat-state
	(update-in [:accepted] conj spotted)
	(update-in [:considering] conj hyp)
	(assoc :hypspace hypspace)
	(add-log-msg time
		     (if (= (:type hyp) "new")
		       (format "Hypothesizing (apriori=%.2f) that %s is new: %s"
			       apriori spotted (:entity hyp))
		       (format "Hypothesizing (apriori=%.2f) that %s is the movement of %s"
			       apriori spotted (:prev hyp)))))))

(defn add-hyp-new
  [strat-state spotted time apriori]
  (let [entity (Entity. [(EntitySnapshot. time (pos spotted))])]
    (add-hyp strat-state time
	     (TrackingHyp. "new" time spotted entity nil)
	     spotted apriori)))

(defn add-hyp-move
  [strat-state spotted time prev apriori]
  (let [event (EventMove. time (pos prev) (pos spotted))]
    (add-hyp strat-state time
	     (TrackingHyp. "move" time spotted nil prev)
	     spotted apriori)))

(defn filter-candidate-entities
  [time entities]
  "Restrict candidate entities to those just hypothesized (for new)
   or one prior (for movements)."
  (filter #(>= 1 (- time (:time (last (:snapshots %))))) entities))

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
      (reduce (fn [ss spotted] (add-hyp-new ss spotted time 0.2)) ;; TODO: real prob of new
	      strat-state unique-spotted)

      ;; got some previously-known entities, so associate them with spotted
      (loop [pairs (pair-near unique-spotted candidate-entities 10) ;; TODO: real walk size
	     ss strat-state]
	(cond (empty? pairs) ss

	      ;; all entities too far for this spotted; hypothesize new entity
	      (empty? (:entities (first pairs)))
	      (recur (rest pairs)
		     (add-hyp-new ss (:spotted (first pairs)) time 0.2)) ;; TODO: see above

	      ;; some entities in range; hypothesize them all, plus a new-entity hyp
	      ;; then make them all mutually conflicting (TODO)
	      :else
	      (recur (rest pairs)
		     (reduce (fn [tempss e]
			       (add-hyp-move tempss (:spotted (first pairs)) time
					     (:entity e) (- 1.0 (/ (:dist e) 10)))) ;; TODO
			     ;; add "new-entity" hyp before hypothesizing the movements
			     (add-hyp-new ss (:spotted (first pairs)) time 0.2)
			     (:entities (first pairs)))))))))

(defn update-problem-data
  [strat-state]
  (loop [accepted (:accepted strat-state)
	 eventlog (:problem-data strat-state)]
    (if	(empty? accepted) (assoc strat-state :problem-data eventlog)
	
	(let [hyp (first accepted)]

	  ;; skip sensor entity types
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

