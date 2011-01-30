(ns samre.problems.tracking.eventlog
  (:require [samre.problems.tracking entities events])
  (:import [samre.problems.tracking.entities Entity EntitySnapshot])
  (:import [samre.problems.tracking.events EventNew EventMove])
  (:use [samre.problems.tracking.entities :only (add-snapshot pos)])
  (:use [samre.colors]))

(defrecord EventLog [events entities])

(defn init-event-log [] (EventLog. #{} #{}))

(defn add-entity
  [eventlog entity]
  (update-in eventlog [:entities] conj entity))

(defn remove-entity
  [eventlog entity]
  (update-in eventlog [:entities] disj entity))

(defn update-entity
  ;;possibly use a reverse-lookup map in the future, to get entity keys
  ;;eg: (let [m {:a :b :c :d}] (zipmap (vals m) (keys m)))
  ([eventlog time entity pos]
     (update-entity eventlog time entity pos (:color entity)))
  ([eventlog time entity pos color]
     (assoc eventlog :entities
            (doall (map #(if (not= (:id %) (:id entity)) %
                             (add-snapshot (assoc % :color
                                                  (if (not= color gray) color (:color %)))
                                           (EntitySnapshot. time pos)))
                        (:entities eventlog))))))

(defn add-event
  [eventlog event]
  (update-in eventlog [:events] conj event))

(defn add-event-new
  [eventlog time pos]
  (add-event eventlog (EventNew. time pos)))

(defn add-event-move
  [eventlog time oldpos pos]
  (add-event eventlog (EventMove. time (dec time) pos oldpos)))

(defn get-entities [eventlog] (:entities eventlog))

(defn get-events [eventlog] (sort-by :time (:events eventlog)))

