(ns simulator.types.states
  (:require [simulator.types entities events])
  (:import [simulator.types.entities Entity EntitySnapshot])
  (:import [simulator.types.events EventNew EventMove])
  (:use [simulator.types.entities :only (add-snapshot pos)]))

(defprotocol StateMethods
  (add-entity [this entity])
  (update-entity [this entity pos])
  (add-event-new [this time pos])
  (add-event-move [this time oldpos newpos])
  (add-event [this event])
  (get-entities [this])
  (get-events [this]))

(defrecord State [events entities]
  StateMethods
  (add-entity
   [this entity]
   (update-in this [:entities] conj
	      (Entity. (if (:symbol entity) (:symbol entity) \X)
		       [(EntitySnapshot. (pos entity))])))
  (update-entity
   ;;possibly use a reverse-lookup map in the future, to get entity keys
   ;;eg: (let [m {:a :b :c :d}] (zipmap (vals m) (keys m)))
   [this entity pos]
   (assoc this :entities
	  (map #(if (not= % entity) %
		    (add-snapshot % (EntitySnapshot. pos)))
	       (:entities this))))
  (add-event-new
   [this time pos]
   (add-event this (EventNew. time pos)))
  (add-event-move
   [this time oldpos newpos]
   (add-event this (EventMove. time oldpos newpos)))
  (add-event
   [this event]
   (update-in this [:events] conj event))
  (get-entities [this] entities)
  (get-events [this] events))

