(ns simulator.problems.tracking.hypotheses
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.problems.tracking.events EventNew EventMove])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos add-snapshot)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity remove-entity add-event update-entity)])
  (:use [simulator.confidences])
  (:use [simulator.types.hypotheses :only
	 (Hypothesis add-explainers get-explainers add-conflicts)])
  (:use [simulator.strategies :only (add-hyp force-acceptance)])
  (:use [clojure.set]))

(defrecord TrackingHyp [id apriori type time spotted entity prev event]
  Hypothesis
  (get-apriori [_] apriori)
  Object
  (toString [_] (format "TrackingHyp %s (a=%d) (%s)@%d\n\t(spotted: %s)\n\t%s\n\t%s"
			id apriori type time spotted
			(if (= type "new") entity prev) event)))

(defn make-hyp-id
  [spotted time prev]
  (format "TH%d%d%d%s%s" (:x (pos spotted)) (:y (pos spotted)) time
	  (if prev (str (:x (pos prev))) "X") (if prev (str (:y (pos prev))) "X")))

(defn pair-near
  "For each spotted, find entities within walk distance."
  [spotted entities walk]
  (let [distfn (fn [s e] {:entity e :dist (manhattan-distance (pos s) (pos e))})]
    (for [s spotted]
      {:spotted s :entities
       (filter #(>= walk (:dist %))
	       (map (partial distfn s) entities))})))

(defn add-hyp-new
  [strat-state spotted time apriori]
  (let [event (EventNew. (:time spotted) (pos spotted))
	entity (Entity. [(EntitySnapshot. time (pos spotted))])
        hyp (TrackingHyp. (make-hyp-id spotted time nil) apriori
                          "new" time spotted entity nil event)
        ss (-> strat-state
               (add-hyp time spotted #{}
                        (format "Hypothesizing spotted %s" (str spotted)))
               (force-acceptance time spotted
                                 (format "Accepting as fact spotted %s" (str spotted))))]
    (add-hyp ss time hyp #{spotted}
             (format "Hypothesizing (a=%d) that %s is new: %s"
                     apriori spotted (:entity hyp)))))

(defn add-hyp-move
  [strat-state spotted time prev apriori]
  (let [event (EventMove. time (pos prev) (pos spotted))
	entity (add-snapshot prev (EntitySnapshot. time (pos spotted)))
        hyp (TrackingHyp. (make-hyp-id spotted time prev) apriori
                          "move" time spotted entity prev event)
        ss (add-hyp strat-state time spotted #{}
                    (format "Hypothesizing spotted %s" (str spotted)))]
    (add-hyp ss time hyp #{spotted}
             (format "Hypothesizing (a=%d) that %s is the movement of %s"
                     apriori spotted (:prev hyp)))))

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
  [strat-state sensors time params]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))
	candidate-entities
	(filter-candidate-entities time (get-entities (:problem-data strat-state)))]
    
    (if (empty? candidate-entities)

      ;; no previously-known entities, hypothesize all as new
      (reduce (fn [ss spotted]
                (add-hyp-new ss spotted time
                             (if (>= 0.5 (:ProbNewEntities params))
                               IMPLAUSIBLE PLAUSIBLE)))
	      strat-state unique-spotted)

      ;; got some previously-known entities, so associate them with spotted
      (loop [pairs (pair-near unique-spotted candidate-entities (:MaxWalk params))
	     ss strat-state]
	(cond (empty? pairs) ss

	      ;; all entities too far for this spotted; hypothesize new entity
	      (empty? (:entities (first pairs)))
	      (recur (rest pairs)
		     (add-hyp-new ss (:spotted (first pairs)) time
                                  (if (>= 0.5 (:ProbNewEntities params))
                                    IMPLAUSIBLE PLAUSIBLE)))

	      ;; some entities in range; hypothesize them all, plus a new-entity hyp
	      ;; then make them all mutually conflicting
	      :else
	      (let [spotted (:spotted (first pairs))
		    es (:entities (first pairs))
		    ss2 (reduce
			 (fn [tempss e]
			   (add-hyp-move tempss spotted time (:entity e)
                                         (case (< (:dist e) (/ (:MaxWalk params) 4))
                                               VERY-PLAUSIBLE
                                               (< (:dist e) (/ (:MaxWalk params) 2))
                                               PLAUSIBLE
                                               :else NEUTRAL)))
			 ss es)
		    ss3 (add-hyp-new ss2 spotted time
                                     (if (>= 0.5 (:ProbNewEntities params))
                                       IMPLAUSIBLE PLAUSIBLE))
		    ssconflicts
		    (add-mutual-conflicts ss3 (get-explainers (:hypspace ss3) spotted))]
		(recur (rest pairs) ssconflicts)))))))

(defn update-problem-data
  [strat-state time]
  (loop [accepted (get (:accepted strat-state) time)
	 rejected (get (:rejected strat-state) time)
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


