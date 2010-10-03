(ns simulator.problems.tracking.hypotheses
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.problems.tracking.events EventNew EventMove EventFrozen])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos add-snapshot)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity remove-entity add-event update-entity)])
  (:use [simulator.confidences])
  (:use [simulator.types.hypotheses :only
	 (Hypothesis add-explainers get-explainers prob-apriori prob-neg-apriori
                     add-conflicts get-hyp-id-str get-hyp-ids-str)])
  (:use [simulator.strategies :only (add-hyp force-acceptance)])
  (:use [clojure.set]))

(defrecord TrackingHyp [id apriori type time spotted entity prev event]
  Hypothesis
  (get-id [_] id)
  (get-apriori [_] apriori)
  Object
  (toString [_] (format "TrackingHyp %s (a=%d) (%s)@%d\n\t(spotted: %s)\n\t%s\n\t%s."
			id apriori type time (get-hyp-id-str spotted)
			(if (not= type "move") entity prev) event)))

(defn make-hyp-id
  [spotted time prev]
  (format "TH%d%d%d%s%s" (:x (pos spotted)) (:y (pos spotted)) time
	  (if prev (str (:x (pos prev))) "X") (if prev (str (:y (pos prev))) "X")))

(defn pair-near
  "For each spotted, find entities within walk distance; exclude non-movements."
  [spotted entities walk]
  (let [distfn (fn [s e] {:entity e :dist (manhattan-distance (pos s) (pos e))})]
    (doall (for [s spotted]
             {:spotted s :entities
              (filter #(and (< 0 (:dist %)) (>= walk (:dist %)))
                      (map (partial distfn s) entities))}))))

(defn find-possibly-frozen
  [spotted entities]
  (filter :entity
          (for [s spotted]
            {:spotted s :entity
             (first (filter (fn [e] (and (= (dec (:time s))
                                            (:time (last (:snapshots e))))
                                         (= (pos s) (pos e))))
                            entities))})))

(defn add-hyp-new
  [strat-state spotted time apriori]
  (let [event (EventNew. (:time spotted) (pos spotted))
	entity (Entity. [(EntitySnapshot. time (pos spotted))])
        hyp (TrackingHyp. (make-hyp-id spotted time nil) apriori
                          "new" time spotted entity nil event)]
    (add-hyp strat-state time hyp #{spotted}
             (format "Hypothesizing %s (a=%d) that %s is new: %s."
                     (get-hyp-id-str hyp) apriori (get-hyp-id-str spotted) entity))))

(defn add-hyp-move
  [strat-state spotted time prev apriori]
  (let [event (EventMove. time (pos prev) (pos spotted))
	entity (add-snapshot prev (EntitySnapshot. time (pos spotted)))
        hyp (TrackingHyp. (make-hyp-id spotted time prev) apriori
                          "move" time spotted entity prev event)]
    (add-hyp strat-state time hyp #{spotted}
             (format "Hypothesizing %s (a=%d) that %s is the movement of %s."
                     (get-hyp-id-str hyp) apriori (get-hyp-id-str spotted) prev))))

(defn add-hyp-frozen
  [strat-state spotted prev time apriori]
  (let [event (EventFrozen. (:time spotted) (pos spotted))
        entity (add-snapshot prev (EntitySnapshot. time (pos spotted)))
        hyp (TrackingHyp. (make-hyp-id spotted time prev) apriori
                          "frozen" time spotted entity prev event)]
    (add-hyp strat-state time hyp #{spotted}
             (format "Hypothesizing %s (a=%d) that %s is the frozen entity %s."
                     (get-hyp-id-str hyp) apriori (get-hyp-id-str spotted) prev))))

(defn add-mutual-conflicts
  [strat-state hyps]
  (reduce (fn [ss hyp] (update-in ss [:hypspace] add-conflicts hyp
				  (difference hyps #{hyp})))
	  strat-state hyps))

(defn filter-candidate-entities
  [time entities]
  "Restrict candidate entities to those just hypothesized (for new)
   or last seen at most three steps prior (for movements or frozen)."
  (filter #(>= 3 (- time (:time (last (:snapshots %))))) entities))

(defn add-mutual-conflict-events
  [strat-state entities time]
  "For each entity, find all hypotheses that were hypothesized at 'time' and
   are events of that entity, and add mutual conflict relations for these
   event hypotheses."
  (reduce
   (fn [ss e]
     (let [events
           (filter
            (fn [h] (and (= (type h)
                            simulator.problems.tracking.hypotheses.TrackingHyp)
                         (or
                          (and
                           (= (type (:event h))
                              simulator.problems.tracking.events.EventMove)
                           (= (pos e) (:oldpos (:event h))))
                          (and
                           (= (type (:event h))
                              simulator.problems.tracking.events.EventFrozen)
                           (= (pos e) (:pos (:event h)))))))
            (get (:hypothesized-at ss) time))]
       (add-mutual-conflicts ss (set events))))
   strat-state entities))

(defn generate-new-hypotheses
  [strat-state spotted time params]
  (reduce (fn [ss spot]
            (add-hyp-new ss spot time (prob-apriori (:ProbNewEntities params))))
          strat-state spotted))

(defn generate-frozen-hypotheses
  [strat-state spotted entities time params]
  (loop [frozen (find-possibly-frozen spotted entities)
         ss strat-state]
    (if (empty? frozen) ss
        (recur (rest frozen)
               (add-hyp-frozen ss (:spotted (first frozen)) (:entity (first frozen)) time
                               (prob-neg-apriori (:ProbMovement params)))))))

(defn generate-movement-hypotheses
  [strat-state spotted entities time params]
  (loop [pairs (pair-near spotted entities (:MaxWalk params))
         ss strat-state]
    (cond (empty? pairs) ss
          
          ;; all entities too far for this spotted; maybe hypothesize new entity
          (empty? (:entities (first pairs)))
          (recur (rest pairs)
                 (if (= 0 (:ProbNewEntities params)) ss
                     (add-hyp-new ss (:spotted (first pairs)) time
                                  (prob-apriori (:ProbNewEntities params)))))
          
          ;; some entities in range; hypothesize them all,
          ;; plus a new-entity hyp (if probnew != 0),
          ;; then make them all mutually conflicting
          :else
          (let [spotted (:spotted (first pairs))
                es (:entities (first pairs))
                ss2 (reduce
                     (fn [tempss e]
                       (add-hyp-move tempss spotted time (:entity e)
                                     (cond (< (:dist e) (/ (:MaxWalk params) 4))
                                           VERY-PLAUSIBLE
                                           (< (:dist e) (/ (:MaxWalk params) 2))
                                           PLAUSIBLE
                                           :else NEUTRAL)))
                     ss es)
                ss3 (if (= 0 (:ProbNewEntities params)) ss2
                        (add-hyp-new ss2 spotted time
                                     (prob-apriori (:ProbNewEntities params))))
                ssconflicts
                (add-mutual-conflicts ss3 (get-explainers (:hypspace ss3) spotted))]
            (recur (rest pairs) ssconflicts)))))

(defn generate-hypotheses
  [strat-state sensors time params]
  (let [unique-spotted
        (set (apply concat (map :spotted sensors)))
	candidate-entities
        (filter-candidate-entities time (get-entities (:problem-data strat-state)))
        
        ;; hypothesize and accept as fact all the spotted
        ss (reduce (fn [ss spotted]
                     (-> ss
                         (add-hyp time spotted #{}
                                  (format "Hypothesizing spotted %s."
                                          (get-hyp-id-str spotted)))
                         (force-acceptance time spotted
                                           (format "Accepting as fact spotted %s."
                                                   (get-hyp-id-str spotted)))))
                   strat-state unique-spotted)
        
        ss2 (if (empty? candidate-entities)

             ;; no previously-known entities, hypothesize all as new
             (generate-new-hypotheses ss unique-spotted time params)
             
             ;; else, got some previously-known entities, so associate them with spotted
             (let [ss-frozen (if (= 1 (:NumberEntities params)) ss
                               (generate-frozen-hypotheses ss unique-spotted
                                                           candidate-entities time params))
                   ss-movements (generate-movement-hypotheses ss-frozen unique-spotted
                                                              candidate-entities
                                                              time params)]
               ss-movements))]
    
    ;; finally, add mutual conflicts for all hypotheses that are an event
    ;; of the same entity
    (add-mutual-conflict-events ss2 candidate-entities time)))

(defn update-problem-data
  [strat-state time]
  (loop [accepted (get (:accepted strat-state) time)
	 eventlog (:problem-data strat-state)]

    (cond (empty? accepted)
	  (assoc strat-state :problem-data eventlog)

	  :else ;; not-empty accepted
	  (let [hyp (first accepted)]

	    ;; skip sensor entity types
	    (if (= (type hyp) simulator.problems.tracking.sensors.SensorEntity)
	      (recur (rest accepted) eventlog)
	      
	      (case (:type hyp)

		    "new"
		    (recur (rest accepted)
			   (-> eventlog
			       (add-event (:event hyp))
			       (add-entity (:entity hyp))))
		    
		    "move"
		    (recur (rest accepted)
			   (-> eventlog
                               (add-event (:event hyp))
			       (remove-entity (:prev hyp))
			       (add-entity (:entity hyp))))

                    "frozen" ;; don't add 'frozen' events to log; just update entity
                    (recur (rest accepted)
                           (-> eventlog
                               (remove-entity (:prev hyp))
                               (add-entity (:entity hyp))))))))))


