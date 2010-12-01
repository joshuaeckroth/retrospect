(ns simulator.problems.tracking.hypotheses
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.problems.tracking.events EventNew EventMove EventFrozen])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos add-snapshot)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity remove-entity add-event update-entity)])
  (:use [simulator.confidences])
  (:use [simulator.hypotheses :only
	 (Hypothesis add-explainers get-explainers prob-apriori prob-neg-apriori
                     get-hyp-id-str get-hyp-ids-str)])
  (:use [simulator.epistemicstates :only
         (add-hyp add-mutual-conflicts
                  add-mutual-conflicts-all-explainers
                  force-acceptance)])
  (:use [simulator.sensors :only (sensed-at)])
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
  [ep-state spotted time apriori]
  (let [event (EventNew. (:time spotted) (pos spotted))
	entity (Entity. [(EntitySnapshot. time (pos spotted))])
        hyp (TrackingHyp. (make-hyp-id spotted time nil) apriori
                          "new" time spotted entity nil event)]
    (add-hyp ep-state hyp #{spotted}
             (format "Hypothesizing %s (a=%d) that %s is new: %s."
                     (get-hyp-id-str hyp) apriori (get-hyp-id-str spotted) entity))))

(defn add-hyp-move
  [ep-state spotted time prev apriori]
  (let [event (EventMove. time (pos prev) (pos spotted))
	entity (add-snapshot prev (EntitySnapshot. time (pos spotted)))
        hyp (TrackingHyp. (make-hyp-id spotted time prev) apriori
                          "move" time spotted entity prev event)]
    (add-hyp ep-state hyp #{spotted}
             (format "Hypothesizing %s (a=%d) that %s is the movement of %s."
                     (get-hyp-id-str hyp) apriori (get-hyp-id-str spotted) prev))))

(defn add-hyp-frozen
  [ep-state spotted prev time apriori]
  (let [event (EventFrozen. (:time spotted) (pos spotted))
        entity (add-snapshot prev (EntitySnapshot. time (pos spotted)))
        hyp (TrackingHyp. (make-hyp-id spotted time prev) apriori
                          "frozen" time spotted entity prev event)]
    (add-hyp ep-state hyp #{spotted}
             (format "Hypothesizing %s (a=%d) that %s is the frozen entity %s."
                     (get-hyp-id-str hyp) apriori (get-hyp-id-str spotted) prev))))

(defn filter-candidate-entities
  [time entities]
  "Restrict candidate entities to those just hypothesized (for new)
   or last seen at most three steps prior (for movements or frozen)."
  (filter #(>= 3 (- time (:time (last (:snapshots %))))) entities))

(defn add-mutual-conflict-events
  [ep-state entities]
  "For each entity, find all hypotheses that were hypothesized at 'time' and
   are events of that entity, and add mutual conflict relations for these
   event hypotheses."
  (reduce
   (fn [es e]
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
            (:hypothesized es))]
       (add-mutual-conflicts es (set events))))
   ep-state entities))

(defn generate-new-hypotheses
  [ep-state spotted time params]
  (reduce (fn [es spot]
            (add-hyp-new es spot time (prob-apriori (:ProbNewEntities params))))
          ep-state spotted))

(defn generate-frozen-hypotheses
  [ep-state spotted entities time params]
  (loop [frozen (find-possibly-frozen spotted entities)
         es ep-state]
    (if (empty? frozen) es
        (recur (rest frozen)
               (add-hyp-frozen es (:spotted (first frozen)) (:entity (first frozen)) time
                               (prob-neg-apriori (:ProbMovement params)))))))

(defn generate-movement-hypotheses
  [ep-state spotted entities time params]
  (loop [pairs (pair-near spotted entities (:MaxWalk params))
         es ep-state]
    (cond (empty? pairs) es
          
          ;; all entities too far for this spotted; maybe hypothesize new entity
          (empty? (:entities (first pairs)))
          (recur (rest pairs)
                 (if (= 0 (:ProbNewEntities params)) es
                     (add-hyp-new es (:spotted (first pairs)) time
                                  (prob-apriori (:ProbNewEntities params)))))
          
          ;; some entities in range; hypothesize them all,
          ;; plus a new-entity hyp (if probnew != 0),
          ;; then make them all mutually conflicting
          :else
          (let [spotted (:spotted (first pairs))
                ents (:entities (first pairs))
                es2 (reduce
                     (fn [tempes e]
                       (add-hyp-move tempes spotted time (:entity e)
                                     (cond (< (:dist e) (/ (:MaxWalk params) 4))
                                           VERY-PLAUSIBLE
                                           (< (:dist e) (/ (:MaxWalk params) 2))
                                           PLAUSIBLE
                                           :else NEUTRAL)))
                     es ents)
                es3 (if (= 0 (:ProbNewEntities params)) es2
                        (add-hyp-new es2 spotted time
                                     (prob-apriori (:ProbNewEntities params))))
                esconflicts
                (add-mutual-conflicts-all-explainers es3 spotted)]
            (recur (rest pairs) esconflicts)))))

(defn generate-hypotheses
  [ep-state sensors params]
  (let [time (:time ep-state)
        unique-spotted
        (set (apply concat (map #(sensed-at % time) sensors)))
	candidate-entities
        (filter-candidate-entities time (get-entities (:problem-data ep-state)))
        
        ;; hypothesize and accept as fact all the spotted
        es (reduce (fn [es spotted]
                     (-> es
                         (add-hyp spotted #{}
                                  (format "Hypothesizing spotted %s."
                                          (get-hyp-id-str spotted)))
                         (force-acceptance spotted
                                           (format "Accepting as fact spotted %s."
                                                   (get-hyp-id-str spotted)))))
                   ep-state unique-spotted)
        
        es2 (if (empty? candidate-entities)

             ;; no previously-known entities, hypothesize all as new
             (generate-new-hypotheses es unique-spotted time params)
             
             ;; else, got some previously-known entities, so associate them with spotted
             (let [es-frozen (if (= 1 (:NumberEntities params)) es
                               (generate-frozen-hypotheses es unique-spotted
                                                           candidate-entities time params))
                   es-movements (generate-movement-hypotheses es-frozen unique-spotted
                                                              candidate-entities
                                                              time params)]
               es-movements))]
    
    ;; finally, add mutual conflicts for all hypotheses that are an event
    ;; of the same entity
    (add-mutual-conflict-events es2 candidate-entities)))

;; needs to be called before reasoning begins at each time step
(defn update-problem-data
  [ep-state]
  (loop [accepted (:accepted ep-state)
	 eventlog (:problem-data ep-state)]

    (cond (empty? accepted)
	  (assoc ep-state :problem-data eventlog)

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


