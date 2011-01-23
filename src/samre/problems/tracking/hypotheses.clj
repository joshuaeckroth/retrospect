(ns samre.problems.tracking.hypotheses
  (:require [samre workspaces])
  (:require [samre.problems.tracking events entities])
  (:import [samre.workspaces Hypothesis])
  (:import [samre.problems.tracking.entities Entity EntitySnapshot])
  (:import [samre.problems.tracking.events EventNew EventMove EventFrozen EventDisappear])
  (:use [samre.problems.tracking.positions :only [manhattan-distance]])
  (:use [samre.problems.tracking.entities :only [pos]])
  (:use [samre.problems.tracking.eventlog :only
	 [get-entities add-entity add-event update-entity]])
  (:use [samre.confidences])
  (:use [samre.epistemicstates :only
         [add-hyp force-acceptance]])
  (:use [samre.sensors :only [sensed-from]])
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
  (format "TrackingHyp %s (a=%s, c=%s)\nEntity: %s\nEvents: %s\nExplains: %s"
          (name (:id hyp)) (confidence-str (:apriori hyp))
          (confidence-str (:confidence hyp))
          (str (:entity (:data hyp)))
          (apply str (interpose "," (map str (:events (:data hyp)))))
          (apply str (interpose "," (sort (map name (:explains hyp)))))))

(defn ancient-fn
  [old-time hyp new-time sb]
  (> (- new-time old-time) sb))

(defn update-fn
  "Given an entity and events, update the event log."
  [entity events pdata]
  ;; TODO: allow multiple updates for same entity; note that the entity
  ;; changes after the first update, so can't use "entity" again
  (assoc pdata :eventlog
         (reduce (fn [el ev] (cond (= (type ev) samre.problems.tracking.events.EventNew)
                                   (-> el
                                       (add-event ev)
                                       (add-entity entity))
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
                                       (update-entity (:time ev) entity (:pos ev)))))
                 (:eventlog pdata) (sort-by :time events))))

(def implausible-fn (constantly []))

(defn impossible-fn
  "Two tracking hypotheses are completely incompatible ('impossible'
  together) when they continue from the same existing entity or
  explain at least one common sensor detection."
  [hyp hyps]
  (let [explains (set (:explains hyp))
        entity (:entity (:data hyp))]
    (filter (fn [h]
              (and
               (not= hyp h)
               (or (= (:entity (:data h)) entity)
                   (not-empty (set/intersection (set (:explains h)) explains)))))
            hyps)))

(defn same-start-or-end
  [link1 link2]
  (or
   (and
    (= (:start-time link1) (:start-time link2))
    (= (:start-pos link1) (:start-pos link2)))
   (and
    (= (:end-time link1) (:end-time link2))
    (= (:end-pos link1) (:end-pos link2)))))

(defn score-event-new
  [event params]
  (if (= (:time event) 0) VERY-PLAUSIBLE (prob-apriori (:ProbNewEntities params))))

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

(defn score-and-sort-links
  "Score single links (which are tracking hypotheses each containing
  one of EventMove, EventNew, EventFrozen), and sort them."
  [available params]
  (let [score (fn [a] (let [t (type (:event a))]
                        (assoc a :conf
                               (cond (= t samre.problems.tracking.events.EventNew)
                                     (score-event-new (:event a) params)
                                     (= t samre.problems.tracking.events.EventFrozen)
                                     (score-event-frozen (:event a) params)
                                     (= t samre.problems.tracking.events.EventMove)
                                     (score-event-move (:event a) params)
                                     (= t samre.problems.tracking.events.EventDisappear)
                                     (score-event-disappear (:event a) params)))))]
    (reverse (sort-by :conf (map score available)))))

(defn score-path
  "Scores an entire path of choices."
  [path]
  (apply min (map :conf path)))

(defn in-range?
  [pos1 pos2 params]
  (<= (manhattan-distance pos1 pos2) (:MaxWalk params)))

(defn generate-initial-movements
  "Generate initial movements from existing entities (s = start, e = end)."
  [entities nodes params]
  (for [s entities e (filter (fn [n] (and (= (inc (:time (last (:snapshots s))))
                                             (:time n))
                                          (not= (pos s) (:pos n))
                                          (in-range? (pos s) (:pos n) params))) nodes)]
    {:id (hash [s (:time e) (:pos e)])
     :event (EventMove. (:time e) (pos s) (:pos e))
     :start-pos (pos s) :end-pos (:pos e)
     :start-time (:time (last (:snapshots s))) :end-time (:time e)
     :entity s :spotted (:spotted e)}))

(defn generate-movements
  "Generate movements between sensor entities."
  [entities nodes params]
  (for [s nodes e (filter (fn [n] (and (= (inc (:time s)) (:time n))
                                       (not= (:pos s) (:pos n))
                                       (in-range? (:pos s) (:pos n) params))) nodes)]
    {:id (hash [(:time s) (:pos s) (:time e) (:pos e)])
     :event (EventMove. (:time e) (:pos s) (:pos e))
     :start-pos (:pos s) :end-pos (:pos e)
     :start-time (:time s) :end-time (:time e)
     :spotted-start (:spotted s) :spotted-end (:spotted e)}))

(defn generate-new
  "Generate new entity events."
  [nodes params time]
  (for [e nodes]
    {:id (hash [(:time e) (:pos e)])
     :event (EventNew. (:time e) (:pos e))
     :start-pos (:pos e) :end-pos (:pos e)
     :start-time (dec (:time e)) :end-time (:time e)
     :entity (Entity. (hash [(:time e) (:x (:pos e)) (:y (:pos e))])
                      [(EntitySnapshot. (:time e) (:pos e))])
     :spotted (:spotted e)}))

(defn generate-frozen
  "Generate frozen entity events which occupy positions of existing entities."
  [entities nodes]
  (for [s entities e (filter (fn [n] (and (= (inc (:time (last (:snapshots s))))
                                             (:time n))
                                          (= (pos s) (:pos n)))) nodes)]
    {:id (hash [s (:time e) (:pos e)])
     :event (EventFrozen. (:time e) (:pos e))
     :start-pos (:pos e) :end-pos (:pos e)
     :start-time (:time (last (:snapshots s))) :end-time (:time e)
     :entity s :spotted (:spotted e)}))

(defn generate-detected-frozen
  "Generate frozen entity events among sensor detections."
  [nodes]
  (for [s nodes e (filter (fn [n] (and (= (inc (:time s)) (:time n))
                                       (= (:pos s) (:pos n)))) nodes)]
    {:id (hash [(:time s) (:pos s) (:time e) (:pos e)])
     :event (EventFrozen. (:time e) (:pos e))
     :start-pos (:pos e) :end-pos (:pos e)
     :start-time (:time s) :end-time (:time e)
     :spotted-start (:spotted s) :spotted-end (:spotted e)}))

(defn generate-disappearances
  "Generate disappearance (and reappearances) when an old detection or entity
   had enough time (2 time steps) and was within range of an unseen area to
   have possibly disappeared and reappeared."
  [entities nodes params pdata]
  (let [ss (filter #(some (fn [p] (in-range? (pos %) p params)) (:sensors-unseen pdata))
                   entities)
        es (filter #(some (fn [p] (in-range? (:pos %) p params)) (:sensors-unseen pdata))
                   nodes)]
    (concat
     ;; s = start, e = end
     ;; from entities to unseen to detected
     (for [s ss e (filter #(<= 2 (- (:time %) (:time (last (:snapshots s))))) es)]
       {:id (hash [s (:time e) (:pos e)])
        :event (EventDisappear. (:time e) (:time (last (:snapshots s))) (:pos e) (pos s))
        :start-pos (pos s) :end-pos (:pos e)
        :start-time (:time (last (:snapshots s))) :end-time (:time e)
        :entity s :spotted (:spotted e)})
     ;; from detected to unseen to detected
     [])))

(defn generate-all-links
  "For each existing entity and each sensor detection, generate the
  following events: a movement event to every sensor detection that is
  one time step ahead; a frozen event at the existing location; and a
  new event at the existing location."
  [entities spotted params time pdata]
  (let [nodes (map (fn [s] {:time (:time (:data s)) :pos (:pos (:data s))
                            :spotted s}) spotted)]
    (concat
     (generate-initial-movements entities nodes params)
     (generate-movements entities nodes params)
     (generate-new nodes params time)
     (generate-frozen entities nodes)
     (generate-detected-frozen nodes)
     (generate-disappearances entities nodes params pdata))))

(def choices-branch? (constantly true))

(defn choices-children [choice] (:children choice))

(defn choices-make-node [choice children] (assoc choice :children children))

(defn choice-to-str
  [choice]
  (format "%s (A=%d)" (str (:event (:link choice))) (count (:available choice))))

(defn choices-to-nested-helper
  [choice]
  (let [deeper (map choices-to-nested-helper (:children choice))]
    (conj deeper (choice-to-str choice))))

(defn choices-to-nested
  [choices]
  (conj (map choices-to-nested-helper (:children (zip/root choices)))
        "root"))

(defn print-choices
  [choices]
  (vijual/draw-tree [(choices-to-nested choices)]))

(defn flatten-choices
  [choices]
  (loop [choices (:children (zip/root choices))
         cs []]
    (if (empty? choices) cs
        (recur (apply concat (map :children choices))
               (concat cs (map (comp :id :link) choices))))))

(defn init-choices
  "Initialize the 'choices' tree with all links available and no link
  chosen."
  [available]
  (zip/zipper choices-branch? choices-children choices-make-node {:available available}))

(defn subtract-from-available
  [choices attempted]
  (let [available (:available (zip/node choices))
        att (map :link attempted)
        remaining-ids (set/difference (set (map :id available))
                                      (set (map :id att)))]
    (reverse (sort-by :conf (filter (fn [a] (some #(= % (:id a)) remaining-ids))
                                    available)))))

(defn choose-next-link
  "Given the 'choices' tree (existing choices) and the available
  linkages, add a new choice node in the tree that contains what is
  currently avaiable and the choice made"
  [choices available]
  (let [link (first available)
        avail (filter (fn [l] (not (same-start-or-end link l))) available)
        choice {:link link :available avail}]
    (zip/rightmost (zip/down (zip/append-child choices choice)))))

(defn construct-remaining-path
  "Given a 'choices' tree (which may or may not have partial paths in
  it), construct the remaining paths."
  [choices]
  (let [available (subtract-from-available choices (zip/children choices))]
    (if (empty? available) choices
        (recur (choose-next-link choices available)))))

(defn maybe-backtrack
  "Given a 'choices' tree, go up to the nearest choice whose
  'available' set has not been exhausted in its children. If no such
  choice exists, return nil; otherwise return the resulting 'choices'
  tree."
  [choices]
  (let [available (subtract-from-available choices (zip/children choices))]
    ;; if no choices are remaining (children have exhausted all choices),
    ;; and if this is the top node, return nil; if this is not the top node,
    ;; go to the parent and try it again
    (if (empty? available)
      (if (nil? (zip/up choices)) nil
          (maybe-backtrack (zip/up choices)))
      ;; the remaining choices is not empty, so this position is acceptable
      choices)))

(defn choices-to-seq
  "Walks up from the current choice to the root, building a sequence
  of choices as it goes. This represents a single consistent (if
  complete) sequence of choices."
  [choices]
  (loop [c choices
         s []]
    (if c (recur (zip/up c) (conj s (:link (zip/node c))))
        s)))

(defn obtain-path-for-entity
  "Given a seq of choices from the 'choices' tree, and an entity,
  obtain the chosen path for that entity by finding, step-by-step, the
  sequence of choices for that entity."
  [choices-seq entity]
  (loop [;; initialize 'path' as the vec containing only the entity's first link
         path (filter #(and % (= (:id (:entity %)) (:id entity))) choices-seq)]
    (if (empty? path) path
        (let [{end-time :end-time end-pos :end-pos} (last path)
              same-start (filter #(and (= (:start-pos %) end-pos)
                                       (= (:start-time %) end-time))
                                 choices-seq)
              ;; 'next' is whatever link starts at end-pos and end-time
              ;; and has endtime is least time of those available
              next (first (sort-by :end-time same-start))]
          (if (nil? next) path
              (recur (conj (vec path) next)))))))

(defn hyps-from-choices-seq
  "Given a seq of choices from the 'choices' tree, make hyps."
  [choices-seq entities]
  (let [es (concat entities (map :entity (filter :entity choices-seq)))
        entity-map (reduce (fn [m e] (assoc m e (obtain-path-for-entity choices-seq e)))
                           {} es)]
    (for [e (filter #(not-empty (entity-map %)) es)]
      (let [events (sort-by :time (map :event (entity-map e)))
            spotted (set (map :id
                              (filter identity (concat (map :spotted (entity-map e))
                                                       (map :spotted-start (entity-map e))
                                                       (map :spotted-end (entity-map e))))))
            apriori (score-path (entity-map e))]
        (Hypothesis. (keyword (format "TH%d" (hash [e events spotted])))
                     :tracking
                     apriori apriori
                     spotted
                     implausible-fn
                     impossible-fn
                     (partial update-fn e events)
                     (partial ancient-fn (:time (last events)))
                     str-fn
                     {:entity e :events events})))))

(defn prepare-hyps
  [ep-state time-prev time-now sensors params]
  (let [spotted-by-sensors (apply concat (map #(sensed-from % time-prev) sensors))
        unique-spotted (vals (apply merge (map (fn [s] {(:id s) s}) spotted-by-sensors)))
        entities (get-entities (:eventlog (:problem-data ep-state)))
        available-unsorted (generate-all-links entities unique-spotted
                                               params time-now (:problem-data ep-state))
        available (score-and-sort-links available-unsorted params)
        choices (init-choices available)
        ;; hypothesize and state as fact the sensor detections
        ep (reduce (fn [ep s] (-> ep (add-hyp s) (force-acceptance s)))
                   ep-state unique-spotted)
        ep-time (assoc ep :time time-now)]
    (update-in ep-time [:problem-data] assoc :choices choices)))

(defn get-hyps
  [ep-state]
  (let [c (construct-remaining-path (:choices (:problem-data ep-state)))
        c-seq (choices-to-seq c)
        hyps (hyps-from-choices-seq c-seq
                                    (get-entities (:eventlog (:problem-data ep-state))))]
    (update-in (reduce (fn [ep h] (add-hyp ep h)) ep-state hyps)
               [:problem-data] assoc :choices c)))

(defn get-more-hyps
  [ep-state sensors params lazy]
  (if-let [backtracked (maybe-backtrack (:choices (:problem-data ep-state)))]
    (let [ep (update-in ep-state [:problem-data] assoc :choices backtracked)]
      (if-not lazy (recur (get-hyps ep) sensors params lazy)
              (get-hyps ep)))
    ;; we're here if we can't backtrack
    (do
      (comment (print-choices (:choices (:problem-data ep-state))))
      (comment (println "nodes in choices tree: "
                        (count (flatten-choices (:choices (:problem-data ep-state))))))
      ep-state)))
