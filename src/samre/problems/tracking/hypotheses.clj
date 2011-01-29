(ns samre.problems.tracking.hypotheses
  (:require [samre workspaces])
  (:require [samre.problems.tracking events entities])
  (:import [samre.workspaces Hypothesis])
  (:import [samre.problems.tracking.events
            EventNew EventMove EventFrozen EventDisappear EventAppear])
  (:use [samre.problems.tracking.positions :only [manhattan-distance]])
  (:use [samre.problems.tracking.entities :only [new-entity pos]])
  (:use [samre.problems.tracking.eventlog :only
	 [get-entities add-entity add-event update-entity]])
  (:use [samre.problems.tracking.sensors :only [make-spotted-whereto-hyps]])
  (:use [samre.confidences])
  (:use [samre.epistemicstates :only
         [add-hyp force-acceptance]])
  (:use [samre.sensors :only [sensed-from]])
  (:use [samre.colors :only [match-color?]])
  (:use [clojure.set :as set :only [intersection difference]])
  (:require [clojure.zip :as zip])
  (:use [clojure.contrib.math :as math :only [ceil]])
  (:require [vijual :as vijual]))

;; The technique for generating tracking hypothesis is the following.
;;
;; The hypothesis generation function is tasked with generating
;; hypotheses linking existing entities with sensor detections
;; (locations of entities); these sensor detections may span multiple
;; time steps. The hypotheses that are generated represent paths from
;; existing believed entity locations through several sensor
;; detections. No path can represent more than one location at a
;; single time step, and no path can skip a time step. The paths are
;; made up of 'Events' so possible events in the path are EventNew,
;; EventMove, and EventFrozen.
;;
;; There may be many possible paths each entity may have followed; we
;; want to generate the most plausible paths first. The method
;; implemented here is the 'islands of confidence' technique where the
;; most plausible events are secured first and restrict choices for
;; the remaining links. The algorithm is basically as follows: First
;; score all possible linkages (links are only forward in time unless
;; they are EventFrozen). Then pick the most confident link (or a
;; random link among the set of equally most confident), and add this
;; choice to a tree data structure representing the progression of
;; choices made. Then, reduce the set of available links by filtering
;; out all links that have the same start or end of already-chosen
;; links. Repeat the process by choosing the most confident link in
;; the remaining set.
;;
;; The tree data structure that is built contains all of the choices;
;; a leaf node in the tree is reached when the set of available
;; linkages is empty. This only occurs when all sensor detections have
;; been associated to paths from each entity; every entity will have a
;; complete path, no entities will share paths, and no path will loop
;; or go backwards in time. If more hypotheses need to be generated at
;; a later time, this tree can be used in a backtracking approach,
;; where the next most likely configuration (set of hypotheses; that
;; is, set of complete entity paths) is obtained by a minimal
;; backtracking (take back a decision, make a different decision if
;; one is available; otherwise, take back another decision,
;; etc.). Each node in the tree contains the choice and the set of
;; available linkages after the choice is made.
;;
;; The runtime complexity of this approach should be (with n = number
;; of sensor detections) O(n^2) to generate the initial confidence
;; scores (which are not recalculated), and O(n) to build a set of
;; paths (the tree would have no branching since no backtracking has
;; been performed yet). The set of available linkages need not be
;; sorted more than once (which takes O(n*log(n)).

(defn str-fn
  [hyp]
  (format "TrackingHyp %s (a=%s, c=%s)\nEntity: %s\nEvent: %s\nExplains: %s"
          (name (:id hyp)) (confidence-str (:apriori hyp))
          (confidence-str (:confidence hyp))
          (str (:entity (:data hyp)))
          (str (:event (:data hyp)))
          (apply str (interpose "," (sort (map name (:explains hyp)))))))

(defn ancient-fn
  [old-time hyp new-time sb]
  (> (- new-time old-time) sb))

(def implausible-fn (constantly []))

(defn impossible-fn
  "Two tracking hypotheses are completely incompatible ('impossible'
  together) when they explain at least one common sensor detection
  or start from the same position."
  [hyp hyps]
  (let [explains (set (:explains hyp))
        entity (:entity (:data hyp))
        event (:event (:data hyp))]
    (filter (fn [h]
              (let [event-h (:event (:data h))]
                (and
                 (not= hyp h)
                 (= (:type h) :tracking)
                 (or
                  ;; same oldpos/oldtime (ignoring EventNew)?
                  (and (not (nil? (:oldpos event)))
                       (not (nil? (:oldpos event-h)))
                       (= (:x (:oldpos event)) (:x (:oldpos event-h)))
                       (= (:y (:oldpos event)) (:y (:oldpos event-h)))
                       (= (:oldtime event) (:oldtime event-h)))
                  ;; have at least one common explainer
                  (not-empty (set/intersection (set (:explains h)) explains))))))
            hyps)))

(defn in-range?
  ([pos1 pos2 n params]
     (<= (manhattan-distance pos1 pos2) (* n (:MaxWalk params))))
  ([pos1 pos2 params]
     (<= (manhattan-distance pos1 pos2) (:MaxWalk params))))

(defn score-event-new
  [event params]
  (if (= (:time event) 0) VERY-PLAUSIBLE (prob-apriori (:ProbNewEntities params))))

(defn score-event-appear
  [event params]
  NEUTRAL)

(defn score-event-frozen
  [event params]
  (prob-neg-apriori (:ProbMovement params)))

(defn score-event-move
  [event params]
  (let [dist (manhattan-distance (:oldpos event) (:pos event))]
    (cond (<= dist (math/ceil (/ (:MaxWalk params) 4)))
          PLAUSIBLE
          (<= dist (math/ceil (/ (:MaxWalk params) 3)))
          NEUTRAL
          (<= dist (math/ceil (/ (:MaxWalk params) 2)))
          IMPLAUSIBLE
          :else ;; equiv to <= dist (:MaxWalk params)
          VERY-IMPLAUSIBLE)))

(defn score-event-disappear
  [event params]
  NEUTRAL)

(defn score-event
  [event params]
  (let [t (type event)]
    (cond (= t samre.problems.tracking.events.EventNew)
          (score-event-new event params)
          (= t samre.problems.tracking.events.EventAppear)
          (score-event-appear event params)
          (= t samre.problems.tracking.events.EventFrozen)
          (score-event-frozen event params)
          (= t samre.problems.tracking.events.EventMove)
          (score-event-move event params)
          (= t samre.problems.tracking.events.EventDisappear)
          (score-event-disappear event params))))

(defn make-hyp
  [h entity event explains params]
  (let [apriori (score-event event params)]
    (Hypothesis. (keyword (format "TH%d" (hash [(rand) h])))
                 :tracking
                 apriori apriori
                 (map :id explains)
                 implausible-fn
                 impossible-fn
                 (partial ancient-fn (:time event))
                 str-fn
                 {:entity entity :event event})))

(defn make-entity-whereto-hyps
  [entities time-prev]
  (let [mk-hyp (fn [e] (Hypothesis. (keyword (format "TW%d" (hash [(rand) e])))
                                    :entity-whereto VERY-PLAUSIBLE VERY-PLAUSIBLE
                                    [] (constantly []) (constantly [])
                                    (partial ancient-fn (inc (:time (last (:snapshots e)))))
                                    str-fn {:entity e}))]
    (map (fn [e] {:entity e :hyp (mk-hyp e)})
         (filter #(= time-prev (inc (:time (last (:snapshots %))))) entities))))

(defn generate-initial-movements
  "Generate initial movements from existing entities (s = start, e = end)."
  [es-with-hyps nodes params]
  (for [s es-with-hyps e (filter (fn [n] (and
                                          (match-color? (:entity s) n)
                                          (= (inc (:time (last (:snapshots (:entity s)))))
                                             (:time n))
                                          (not= (pos (:entity s)) (:pos n))
                                          (in-range? (pos (:entity s)) (:pos n) params)))
                                 nodes)]
    (let [event (EventMove. (:time e) (dec (:time e)) (:pos e) (pos (:entity s)))]
      (make-hyp [(:entity s) (:time e) (:pos e)]
                (:entity s) event [(:hyp s) (:spotted e)] params))))

(defn generate-movements
  "Generate movements between sensor entities."
  [nodes params]
  (for [s nodes e (filter (fn [n] (and
                                   (match-color? s n)
                                   (= (inc (:time s)) (:time n))
                                   (not= (:pos s) (:pos n))
                                   (in-range? (:pos s) (:pos n) params))) nodes)]
    (let [event (EventMove. (:time e) (dec (:time e)) (:pos e) (:pos s))]
      (make-hyp [s (:time s) (:pos s) (:time e) (:pos e)]
                nil event [(:spotted e) (:hyp s)] params))))

(defn generate-new
  "Generate new entity events."
  [nodes params]
  (for [e nodes]
    (let [event (EventNew. (:time e) (:pos e))]
      (make-hyp [(:time e) (:pos e) "new"] (new-entity (:time e) (:pos e) (:color e))
                event [(:spotted e)] params))))

(defn generate-appear
  "Generate entity appearances."
  [nodes params pdata]
  (for [e (filter #(some (fn [p] (in-range? (:pos %) p params))
                         (:sensors-unseen pdata)) nodes)]
    (let [event (EventAppear. (:time e) (:pos e))]
      (make-hyp [(:time e) (:pos e) "appear"] (new-entity (:time e) (:pos e) (:color e))
                event [(:spotted e)] params))))

(defn generate-frozen
  "Generate frozen entity events which occupy positions of existing entities."
  [es-with-hyps nodes params]
  (for [s es-with-hyps e (filter (fn [n] (and
                                          (match-color? (:entity s) n)
                                          (= (inc (:time (last (:snapshots (:entity s)))))
                                             (:time n))
                                          (= (pos (:entity s)) (:pos n)))) nodes)]
    (let [event (EventFrozen. (:time e) (dec (:time e)) (:pos e) (:pos e))]
      (make-hyp [(:entity s) (:time e) (:pos e)]
                (:entity s) event [(:hyp s) (:spotted e)] params))))

(defn generate-detected-frozen
  "Generate frozen entity events among sensor detections."
  [nodes params]
  (for [s nodes e (filter (fn [n] (and
                                   (match-color? s n)
                                   (= (inc (:time s)) (:time n))
                                   (= (:pos s) (:pos n)))) nodes)]
    (let [event (EventFrozen. (:time e) (dec (:time e)) (:pos e) (:pos e))]
      (make-hyp [(:time s) (:pos s) (:time e) (:pos e)]
                nil event [(:spotted e) (:hyp s)] params))))

(defn generate-disappearances
  "Generate disappearance (and reappearances) when an old detection or entity
   had enough time (2 time steps) and was within range of an unseen area to
   have possibly disappeared and reappeared."
  [es-with-hyps nodes params pdata]
  (let [ss (filter #(some (fn [p] (in-range? (pos (:entity %)) p params))
                          (:sensors-unseen pdata))
                   es-with-hyps)
        es (filter #(some (fn [p] (in-range? (:pos %) p params)) (:sensors-unseen pdata))
                   nodes)
        close-entity (fn [s e] (let [dt (- (:time e) (:time (last (:snapshots s))))]
                                 (and (match-color? s e) (<= 2 dt)
                                      (in-range? (pos s) (:pos e) dt params))))
        close-spotted (fn [s e] (let [dt (- (:time e) (:time s))]
                                  (and (match-color? s e) (<= 2 dt)
                                       (in-range? (:pos s) (:pos e) dt params))))]
    (concat
     ;; s = start, e = end
     ;; from entities to unseen to detected
     (for [s ss e (filter #(close-entity (:entity s) %) es)]
       (let [event (EventDisappear. (:time e) (:time (last (:snapshots (:entity s))))
                                    (:pos e) (pos (:entity s)))]
         (make-hyp [(:entity s) (:time e) (:pos e)]
                   (:entity s) event [(:hyp s) (:spotted e)] params)))
     ;; from detected to unseen to detected
     (for [s es e (filter #(close-spotted s %) es)]
       (let [event (EventDisappear. (:time e) (:time s) (:pos e) (:pos s))]
         (make-hyp [(:time s) (:pos s) (:time e) (:pos e)]
                   nil event [(:spotted e) (:hyp s)] params))))))

(defn generate-all-links
  "For each existing entity and each sensor detection, generate the
  following events: a movement event to every sensor detection that is
  one time step ahead; a frozen event at the existing location; and a
  new event at the existing location."
  [es-with-hyps spotted params time pdata]
  (let [nodes (map (fn [{s :spotted h :hyp}]
                     {:time (:time (:data s)) :pos (:pos (:data s))
                      :color (:color (:data s)) :spotted s :hyp h}) spotted)]
    (concat
     (generate-initial-movements es-with-hyps nodes params)
     (generate-movements nodes params)
     (generate-new nodes params)
     (generate-appear nodes params pdata)
     (generate-frozen es-with-hyps nodes params)
     (generate-detected-frozen nodes params)
     (generate-disappearances es-with-hyps nodes params pdata))))

(defn get-hyps
  [ep-state time-prev time-now sensors params]
  (let [spotted-by-sensors (apply concat (map #(sensed-from % time-prev) sensors))
        unique-spotted (vals (apply merge (map (fn [s] {(:id s) s}) spotted-by-sensors)))
        spotted-with-hyps (make-spotted-whereto-hyps unique-spotted time-now)
        entities (get-entities (:eventlog (:problem-data ep-state)))
        ;; hypothesize and state as fact the sensor detections and whereto hyps
        ep (reduce (fn [ep {s :spotted h :hyp}]
                     (let [ep2 (-> ep (add-hyp s) (force-acceptance s))]
                       (if h (-> ep2 (add-hyp h) (force-acceptance h)) ep2)))
                   ep-state spotted-with-hyps)
        es-with-hyps (make-entity-whereto-hyps entities time-prev)
        ep-entity-hyps (reduce (fn [ep eh] (-> ep (add-hyp (:hyp eh))
                                               (force-acceptance (:hyp eh))))
                               ep es-with-hyps)
        ;; hypothesize the hyps
        hyps (generate-all-links
              es-with-hyps spotted-with-hyps params time-now (:problem-data ep-entity-hyps))
        ep-hyps (reduce add-hyp ep-entity-hyps hyps)
        ep-time (assoc ep-hyps :time time-now)]
    ep-time))

(defn get-more-hyps
  [ep-state time-prev time-now sensors params lazy]
  (get-hyps ep-state time-prev time-now sensors params))

(defn update-eventlog
  "Given an entity and events (in hyps), update the event log."
  [entity hyps pdata]
  (assoc pdata :eventlog
         (reduce (fn [el hyp]
                   (let [e (:entity (:data hyp))
                         ev (:event (:data hyp))]
                     (cond
                      (= (type ev) samre.problems.tracking.events.EventNew)
                      (-> el
                          (add-event ev)
                          (add-entity e))
                      (= (type ev) samre.problems.tracking.events.EventAppear)
                      (-> el
                          (add-event ev)
                          (add-entity e))
                      (= (type ev) samre.problems.tracking.events.EventFrozen)
                      (-> el
                          (update-entity (:time ev) entity (:pos ev)))
                      (= (type ev) samre.problems.tracking.events.EventMove)
                      (-> el
                          (add-event ev)
                          (update-entity (:time ev) entity (:pos ev)))
                      (= (type ev) samre.problems.tracking.events.EventDisappear)
                      (-> el
                          (add-event ev)
                          (update-entity (:time ev) entity (:pos ev))))))
                 (:eventlog pdata) (sort-by (comp :time :event :data) hyps))))

(defn connect-hyps
  [hyps entity]
  (loop [path (vec (filter #(= (:id (:entity (:data %))) (:id entity)) hyps))]
    (if (empty? path) []
        (let [event (:event (:data (last path)))
              end-time (:time event)
              end-pos (:pos event)
              same-start (filter #(and (= (:oldpos (:event (:data %))) end-pos)
                                       (= (:oldtime (:event (:data %))) end-time))
                                 hyps)
              next (first (sort-by (comp :time :event :data) same-start))]
          (if (nil? next) path
              (recur (conj path next)))))))

(defn accept-decision
  [pdata hyps]
  (let [new-entities (map (comp :entity :data)
                          (filter #(or (= (type (:event (:data %)))
                                          samre.problems.tracking.events.EventNew)
                                       (= (type (:event (:data %)))
                                          samre.problems.tracking.events.EventAppear))
                                  hyps))]
    (reduce (fn [pd entity] (update-eventlog entity (connect-hyps hyps entity) pd))
            pdata (concat new-entities (get-entities (:eventlog pdata))))))
