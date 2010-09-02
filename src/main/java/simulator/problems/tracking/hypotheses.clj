(ns simulator.problems.tracking.hypotheses
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.problems.tracking.events EventNew EventMove])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos add-snapshot)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity remove-entity add-event update-entity)])
  (:use [simulator.types.hypotheses :only
	 (add-explainers get-explainers add-conflicts set-apriori)])
  (:use [simulator.strategies :only (add-log-msg)])
  (:use [clojure.set]))

(defrecord TrackingHyp [type time spotted entity prev event]
  Object
  (toString [_] (format "TrackingHyp (%s)@%d (spotted: %s) %s - %s"
			type time spotted
			(if (= type "new") entity prev) event)))

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
  (let [event (EventNew. (:time spotted) (pos spotted))
	entity (Entity. [(EntitySnapshot. time (pos spotted))])]
    (add-hyp strat-state time
	     (TrackingHyp. "new" time spotted entity nil event)
	     spotted apriori)))

(defn add-hyp-move
  [strat-state spotted time prev apriori]
  (let [event (EventMove. time (pos prev) (pos spotted))
	entity (add-snapshot prev (EntitySnapshot. time (pos spotted)))]
    (add-hyp strat-state time
	     (TrackingHyp. "move" time spotted entity prev event)
	     spotted apriori)))

(defn add-mutual-conflicts
  [strat-state hyps]
  (reduce (fn [ss hyp] (update-in ss [:hypspace] add-conflicts hyp
				  (difference hyps #{hyp})))
	  strat-state hyps))

(defn filter-candidate-entities
  [time entities]
  "Restrict candidate entities to those just hypothesized (for new)
   or one prior (for movements)."
  (filter #(>= 1 (- time (:time (last (:snapshots %))))) entities))

(defn generate-hypotheses
  [strat-state sensors time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))
	candidate-entities
	(filter-candidate-entities time (get-entities (:problem-data strat-state)))]
    (doseq [e (get-entities (:problem-data strat-state))] (println (str e)))
    (println (count candidate-entities))
    (doseq [e candidate-entities] (println (str e)))
    
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
	      (let [spotted (:spotted (first pairs))
		    es (:entities (first pairs))
		    ss2 (reduce
			 (fn [tempss e]
			   (add-hyp-move tempss spotted time (:entity e)
					 (- 1.0 (/ (:dist e) 10)))) ;; TODO
			 ss es)
		    ss3 (add-hyp-new ss2 spotted time 0.2)
		    ssconflicts
		    (add-mutual-conflicts ss3 (get-explainers (:hypspace ss3) spotted))]
		(recur (rest pairs) ssconflicts)))))))

(defn update-problem-data
  [strat-state]
  (loop [accepted (:accepted strat-state)
	 rejected (:rejected strat-state)
	 eventlog (:problem-data strat-state)]

    (cond (and (empty? accepted) (empty? rejected))
	  (assoc strat-state :problem-data eventlog)

	  (not-empty rejected)
	  (let [hyp (first rejected)]

	    ;; skip sensor entity types
	    (if (= (type hyp) simulator.problems.tracking.sensors.SensorEntity)
	      (recur accepted (rest rejected) eventlog)
	      (recur accepted (rest rejected) (remove-entity eventlog (:entity hyp)))))

	  :else ;; not-empty accepted
	  (let [hyp (first accepted)]

	    ;; skip sensor entity types
	    (if (= (type hyp) simulator.problems.tracking.sensors.SensorEntity)
	      (recur (rest accepted) rejected eventlog)
	      
	      (case (:type hyp)
		    
		    "new"
		    (recur (rest accepted)
			   rejected
			   (-> eventlog
			       (add-event (:event hyp))
			       (add-entity (:entity hyp))))
		    
		    "move"
		    (recur (rest accepted)
			   rejected
			   (-> eventlog (add-event (:event hyp))
			       (remove-entity (:prev hyp))
			       (add-entity (:entity hyp))))))))))

(defn clear-considering
  [strat-state]
  (assoc strat-state :hypspace (assoc (:hypspace strat-state) :considering #{})))

