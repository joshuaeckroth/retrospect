(ns simulator.problems.tracking.hypotheses
  (:require [simulator workspaces])
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.workspaces Hypothesis])
  (:import [simulator.problems.tracking.events EventNew EventMove EventFrozen])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:use [simulator.problems.tracking.positions :only (manhattan-distance)])
  (:use [simulator.problems.tracking.entities :only (pos add-snapshot)])
  (:use [simulator.problems.tracking.eventlog :only
	 (get-entities add-entity remove-entity add-event update-entity)])
  (:use [simulator.confidences])
  (:use [simulator.epistemicstates :only
         [add-hyp force-acceptance]])
  (:use [simulator.sensors :only (sensed-from)])
  (:use [clojure.set])
  (:use [clojure.contrib.math :as math :only [ceil]]))

(defn tracking-hyp-to-str
  [hyp]
  (format "TrackingHyp %s (a=%d) (%s)@%d\n\t(spotted: %s)\n\t%s\n\t%s."
          (name (:id hyp)) (confidence-str (:apriori hyp))
          (name (:type hyp)) (:time (:data hyp))
          (name (first (:explains hyp)))
          (if (not= (:type hyp) :tracking-move) (:entity (:data hyp)) (:prev (:data hyp)))
          (:event (:data hyp))))

(defn make-hyp-id
  [spotted time prev]
  (keyword (format "TH%d%d%d%s%s" (:x (:pos (:data spotted))) (:y (:pos (:data spotted))) time
                   (if prev (str (:x (pos prev))) "X") (if prev (str (:y (pos prev))) "X"))))

(defn same-start-or-end
  [hyp1 hyp2]
  (let [event1 (:event (:data hyp1))
        event2 (:event (:data hyp2))
        oldpos1 (if (:oldpos event1) (:oldpos event1) (:pos event1))
        oldpos2 (if (:oldpos event2) (:oldpos event2) (:pos event2))
        either-new? (or (= (:type hyp1) :tracking-new) (= (:type hyp2) :tracking-new))]
    (and
     ;; not same hypothesis
     (not (= (:id hyp1) (:id hyp2)))
     ;; same new position at same time
     (or (and (= (:time event1) (:time event2)) (= (:pos event1) (:pos event2)))
         ;; or neither event is new and they have the same previous position at same time
         (and (not either-new?) (= oldpos1 oldpos2) (= (:time event1) (:time event2)))))))

(defn find-conflicts
  "Find tracking hypotheses that end up at same position or originate from same position."
  [hyp hyps]
  (let [tracking-hyps (doall
                       (filter (fn [h] (some #(= (:type h) %)
                                             [:tracking-new :tracking-move :tracking-frozen]))
                               hyps))]
    (doall (filter (partial same-start-or-end hyp) tracking-hyps))))

(defn ancient-fn
  [old-time hyp new-time]
  (> (- new-time old-time) 1))

(defn add-hyp-new
  [ep-state spotted time apriori]
  (let [event (EventNew. time (:pos (:data spotted)))
	entity (Entity. [(EntitySnapshot. time (:pos (:data spotted)))])
        hyp (Hypothesis. (make-hyp-id spotted time nil)
                         :tracking-new
                         apriori apriori
                         [(:id spotted)]
                         (constantly [])
                         find-conflicts
                         (fn [el] (-> el
                                      (add-event event)
                                      (add-entity entity)))
                         (partial ancient-fn time)
                         tracking-hyp-to-str
                         {:time time :entity entity :event event})]
    (add-hyp ep-state hyp
             (format "Hypothesizing %s (a=%d) that %s is new: %s."
                     (name (:id hyp)) apriori (name (:id spotted)) entity))))

(defn add-hyp-move
  [ep-state spotted time prev apriori]
  (let [event (EventMove. time (pos prev) (:pos (:data spotted)))
	entity (add-snapshot prev (EntitySnapshot. time (:pos (:data spotted))))
        hyp (Hypothesis. (make-hyp-id spotted time prev)
                         :tracking-move
                         apriori apriori
                         [(:id spotted)]
                         (constantly [])
                         find-conflicts
                         (fn [el] (-> el
                                      (add-event event)
                                      (remove-entity prev)
                                      (add-entity entity)))
                         (partial ancient-fn time)
                         tracking-hyp-to-str
                         {:time time :entity entity :event event :prev prev})]
    (add-hyp ep-state hyp
             (format "Hypothesizing %s (a=%d) that %s is the movement of %s."
                     (name (:id hyp)) apriori (name (:id spotted)) prev))))

(defn add-hyp-frozen
  [ep-state spotted prev time apriori]
  (let [event (EventFrozen. time (:pos (:data spotted)))
        entity (add-snapshot prev (EntitySnapshot. time (:pos (:data spotted))))
        hyp (Hypothesis. (make-hyp-id spotted time prev)
                         :tracking-frozen
                         apriori apriori
                         [(:id spotted)]
                         (constantly [])
                         find-conflicts
                         (fn [el] (-> el
                                      (remove-entity prev)
                                      (add-entity entity)))
                         (partial ancient-fn time)
                         tracking-hyp-to-str
                         {:time time :entity entity :event event :prev prev})]
    (add-hyp ep-state hyp
             (format "Hypothesizing %s (a=%d) that %s is the frozen entity %s."
                     (name (:id hyp)) apriori (name (:id spotted)) prev))))

(defn filter-candidate-entities
  [time entities]
  "Restrict candidate entities to those just hypothesized (for new)
   or last seen at most three steps prior (for movements or frozen)."
  (doall (filter #(= (- time (:time (last (:snapshots %))))) entities)))

(defn generate-new-hypotheses
  [ep-state spotted time params]
  (let [apriori (if (= time 0) VERY-PLAUSIBLE
                    (prob-apriori (:ProbNewEntities params)))]
    (doall (reduce (fn [es spot]
                     (add-hyp-new es spot time apriori))
                   ep-state spotted))))

(defn find-possibly-frozen
  [spotted entities]
  (doall (filter :entity
                 (for [s spotted]
                   {:spotted s :entity
                    (first (filter (fn [e] (and (= (dec (:time (:data s)))
                                                   (:time (last (:snapshots e))))
                                                (= (:pos (:data s)) (pos e))))
                                   entities))}))))

(defn generate-frozen-hypotheses
  [ep-state spotted entities time params]
  (loop [frozen (find-possibly-frozen spotted entities)
         es ep-state]
    (if (empty? frozen) es
        (recur (rest frozen)
               (add-hyp-frozen es (:spotted (first frozen)) (:entity (first frozen)) time
                               (prob-neg-apriori (:ProbMovement params)))))))

(defn pair-near
  "For each spotted, find entities within walk distance; exclude non-movements."
  [spotted entities walk]
  (let [distfn (fn [s e] {:entity e :dist (manhattan-distance (:pos (:data s)) (pos e))})]
    (doall (for [s spotted]
             {:spotted s :entities
              (filter #(and (< 0 (:dist %)) (>= walk (:dist %)))
                      (map (partial distfn s) entities))}))))

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
                                     (cond (<= (:dist e) (math/ceil (/ (:MaxWalk params) 4)))
                                           VERY-PLAUSIBLE
                                           (<= (:dist e) (math/ceil (/ (:MaxWalk params) 3)))
                                           PLAUSIBLE
                                           (<= (:dist e) (math/ceil (/ (:MaxWalk params) 2)))
                                           IMPLAUSIBLE
                                           (<= (:dist e) (:MaxWalk params))
                                           VERY-IMPLAUSIBLE)))
                     es ents)
                es3 (if (= 0 (:ProbNewEntities params)) es2
                        (add-hyp-new es2 spotted time
                                     (prob-apriori (:ProbNewEntities params))))]
            (recur (rest pairs) es3)))))

;; TODO: fix to generate hypotheses from time onwards (as much as sensors provide)
(defn generate-hypotheses
  [ep-state sensors params]
  (let [time (:time ep-state)
        spotted-by-sensors (apply concat (map #(sensed-from % time) sensors))
        unique-spotted
        (vals (apply merge (map (fn [s] {(:id s) s}) spotted-by-sensors)))
	candidate-entities
        (filter-candidate-entities time (get-entities (:problem-data ep-state)))
        
        ;; hypothesize and accept as fact all the spotted
        es (doall (reduce (fn [es spotted]
                            (-> es
                                (add-hyp spotted (format "Hypothesizing spotted %s."
                                                         (name (:id spotted))))
                                (force-acceptance spotted
                                                  (format "Accepting as fact spotted %s."
                                                          (name (:id spotted))))))
                          ep-state unique-spotted))
        
        es2 (if (empty? candidate-entities)

             ;; no previously-known entities, hypothesize all as new
             (generate-new-hypotheses es unique-spotted time params)
             
             ;; else, got some previously-known entities, so associate them with spotted
             (let [es-frozen (generate-frozen-hypotheses es unique-spotted
                                                         candidate-entities time params)
                   es-movements (generate-movement-hypotheses es-frozen unique-spotted
                                                              candidate-entities
                                                              time params)]
               es-movements))]
    (println "candidate entities: " (count candidate-entities))
    es2))


