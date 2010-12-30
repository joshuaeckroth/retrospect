(ns simulator.problems.tracking.hypotheses
  (:require [simulator workspaces])
  (:require [simulator.problems.tracking events entities])
  (:import [simulator.workspaces Hypothesis])
  (:import [simulator.problems.tracking.entities Entity EntitySnapshot])
  (:import [simulator.problems.tracking.events EventNew EventMove EventFrozen])
  (:use [simulator.problems.tracking.positions :only [manhattan-distance]])
  (:use [simulator.problems.tracking.entities :only [pos]])
  (:use [simulator.problems.tracking.eventlog :only
	 [get-entities add-entity add-event update-entity]])
  (:use [simulator.confidences])
  (:use [simulator.epistemicstates :only
         [add-hyp force-acceptance]])
  (:use [simulator.sensors :only [sensed-from]])
  (:use [clojure.set :as set :only [intersection difference]])
  (:require [clojure.zip :as zip])
  (:use [clojure.contrib.math :as math :only [ceil]]))

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
  (format "TrackingHyp %s (a=%s, c=%s)\nEntity: %s\nExplains: %s"
          (name (:id hyp)) (confidence-str (:apriori hyp))
          (confidence-str (:confidence hyp))
          (str (:entity (:data hyp)))
          (apply str (interpose "," (sort (map name (:explains hyp)))))))

(defn ancient-fn
  [old-time hyp new-time]
  (< 3 (- new-time old-time)))

(defn update-fn
  "Given an entity and events, update the event log."
  [entity events eventlog]
  (reduce (fn [el ev] (cond (= (type ev) simulator.problems.tracking.events.EventNew)
                            (-> el
                                (add-event ev)
                                (add-entity entity))
                            (= (type ev) simulator.problems.tracking.events.EventFrozen)
                            (-> el
                                (update-entity (:time ev) entity (:pos ev)))
                            (= (type ev) simulator.problems.tracking.events.EventMove)
                            (-> el
                                (add-event ev)
                                (update-entity (:time ev) entity (:pos ev)))))
          eventlog events))

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
  (and (= (:end-time link1) (:end-time link2))
       (or
        (= (:start-pos link1) (:start-pos link2))
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
          VERY-PLAUSIBLE
          (<= dist (math/ceil (/ (:MaxWalk params) 2)))
          PLAUSIBLE
          (<= dist (math/ceil (/ (:MaxWalk params))))
          IMPLAUSIBLE
          :else
          VERY-IMPLAUSIBLE)))

(defn score-and-sort-links
  "Score single links (which are tracking hypotheses each containing
  one of EventMove, EventNew, EventFrozen), and sort them."
  [available params]
  (let [score (fn [a] (let [t (type (:event a))]
                        (assoc a :conf
                               (cond (= t simulator.problems.tracking.events.EventNew)
                                     (score-event-new (:event a) params)
                                     (= t simulator.problems.tracking.events.EventFrozen)
                                     (score-event-frozen (:event a) params)
                                     (= t simulator.problems.tracking.events.EventMove)
                                     (score-event-move (:event a) params)))))]
    (reverse (sort-by :conf (map score available)))))

(defn score-path
  "Scores an entire path of choices."
  [path]
  (apply min (map :conf path)))

(defn generate-all-links
  "For each existing entity and each sensor detection, generate the
  following events: a movement event to every sensor detection that is
  one time step ahead; a frozen event at the existing location; and a
  new event at the existing location."
  [entities spotted]
  (let [nodes (map (fn [s] {:time (:time (:data s)) :pos (:pos (:data s))
                            :spotted s}) spotted)]
    (concat
     ;; generate initial movements from existing entities (s = start, e = end)
     (for [s entities e (filter (fn [n] (and (= (inc (:time (last (:snapshots s))))
                                                (:time n))
                                             (not= (pos s) (:pos n)))) nodes)]
       {:event (EventMove. (:time e) (pos s) (:pos e))
        :start-pos (pos s) :end-pos (:pos e) :end-time (:time e)
        :entity s :spotted (:spotted e)})
     ;; generate movements between sensor detections
     (for [s nodes e (filter (fn [n] (and (= (inc (:time s)) (:time n))
                                          (not= (:pos s) (:pos n)))) nodes)]
       {:event (EventMove. (:time e) (:pos s) (:pos e))
        :start-pos (:pos s) :end-pos (:pos e) :end-time (:time e)
        :spotted-start (:spotted s) :spotted-end (:spotted e)})
     ;; generate new entity events
     (for [e nodes]
       {:event (EventNew. (:time e) (:pos e))
        :start-pos (:pos e) :end-pos (:pos e) :end-time (:time e)
        :entity (Entity. [(EntitySnapshot. (:time e) (:pos e))])
        :spotted (:spotted e)})
     ;; generate frozen entity events which occupy positions of existing entities
     (for [s entities e (filter (fn [n] (and (= (inc (:time (last (:snapshots s))))
                                                (:time n))
                                             (= (pos s) (:pos n)))) nodes)]
       {:event (EventFrozen. (:time e) (:pos e))
        :start-pos (:pos e) :end-pos (:pos e) :end-time (:time e)
        :entity s :spotted (:spotted e)})
     ;; generate frozen entity events among sensor detections
     (for [s nodes e (filter (fn [n] (and (= (inc (:time s)) (:time n))
                                          (= (:pos s) (:pos n)))) nodes)]
       {:event (EventFrozen. (:time e) (:pos e))
        :start-pos (:pos e) :end-pos (:pos e) :end-time (:time e)
        :spotted-start (:spotted s) :spotted-end (:spotted e)}))))

(def choices-branch? (constantly true))

(defn choices-children [choice] (:children choice))

(defn choices-make-node [choice children] (assoc choice :children children))

(defn init-choices
  "Initialize the 'choices' tree with all links available and no link
  chosen."
  [available]
  (zip/zipper choices-branch? choices-children choices-make-node {:available available}))

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
  (let [available (reverse (sort-by :conf (set/difference
                                           (set (:available (zip/node choices)))
                                           (set (map :link (zip/lefts choices))))))]
    (if (empty? (:available (zip/node choices))) choices
        (recur (choose-next-link choices available)))))

(defn maybe-backtrack
  "Given a 'choices' tree, go up to the nearest choice whose
  'available' set has not been exhausted in its children. If no such
  choice exists, return nil; otherwise return the resulting 'choices'
  tree."
  [choices]
  (let [attempted (map :link (zip/children choices))
        remaining (reverse (sort-by :conf (set/difference
                                           (set (:available (zip/node choices)))
                                           (set attempted))))]
    ;; if no choices are remaining (children have exhausted all choices),
    ;; and if this is the top node, return nil; if this is not the top node,
    ;; go to the parent and try it again
    (if (empty? remaining)
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
    (if c (recur (zip/prev c) (conj s (:link (zip/node c))))
        s)))

(defn obtain-path-for-entity
  "Given a seq of choices from the 'choices' tree, and an entity,
  obtain the chosen path for that entity by finding, step-by-step, the
  sequence of choices for that entity."
  [choices-seq entity]
  (loop [;; initialize 'path' as the vec containing only the entity's first link
         path (filter #(and % (= (:entity %) entity)) choices-seq)]
    (if (empty? path) path
        (let [{end-time :end-time end-pos :end-pos} (last path)
              ;; 'next' is whatever link starts at end-pos and has :endtime=endtime+1
              next (first (filter #(and (= (:start-pos %) end-pos)
                                        (= (:end-time %) (inc end-time)))
                                  choices-seq))]
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
  [ep-state time-prev sensors params]
  (let [spotted-by-sensors (apply concat (map #(sensed-from % time-prev) sensors))
        unique-spotted (vals (apply merge (map (fn [s] {(:id s) s}) spotted-by-sensors)))
        entities (get-entities (:problem-data ep-state))
        available-unsorted (generate-all-links entities unique-spotted)
        available (score-and-sort-links available-unsorted params)
        choices (init-choices available)
        ;; hypothesize and state as fact the sensor detections
        ep (reduce (fn [ep s] (-> ep (add-hyp s) (force-acceptance s)))
                   ep-state unique-spotted)]
    (update-in ep [:problem-data] assoc :choices choices)))

(defn get-more-hyps
  [ep-state sensors params]
  (if-let [backtracked (maybe-backtrack (:choices (:problem-data ep-state)))]
    (let [choices (construct-remaining-path backtracked)
          choices-seq (choices-to-seq choices)
          hyps (hyps-from-choices-seq choices-seq (get-entities (:problem-data ep-state)))]
      (reduce (fn [ep h] (add-hyp ep h)) ep-state hyps))
    ;; we're here if we can't backtrack
    ep-state))
