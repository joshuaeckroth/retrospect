(ns simulator.types.states
  (:require [simulator.types entities events logs])
  (:import [simulator.types.entities Entity EntitySnapshot])
  (:import [simulator.types.events EventNew EventMove])
  (:import [simulator.types.logs LogEntry])
  (:use [simulator.types.entities :only (addSnapshot pos)]))

(defprotocol StateMethods
  (addEntity [this entity])
  (updateEntity [this entity pos])
  (addEventNew [this time pos])
  (addEventMove [this time oldpos newpos])
  (addEvent [this event])
  (addLog [this time msg])
  (formatLogs [this]))

(defrecord State [events entities logs]
  StateMethods
  (addEntity
   [this entity]
   (update-in this [:entities] conj
	      (Entity. (if (:symbol entity) (:symbol entity) \X)
		       [(EntitySnapshot. (pos entity))])))
  (updateEntity
   ;;possibly use a reverse-lookup map in the future, to get entity keys
   ;;eg: (let [m {:a :b :c :d}] (zipmap (vals m) (keys m)))
   [this entity pos]
   (assoc this :entities
	  (map #(if (not= % entity) %
		    (addSnapshot % (EntitySnapshot. pos)))
	       (:entities this))))
  (addEventNew
   [this time pos]
   (addEvent this (EventNew. time pos)))
  (addEventMove
   [this time oldpos newpos]
   (addEvent this (EventMove. time oldpos newpos)))
  (addEvent
   [this event]
   (update-in this [:events] conj event))
  (addLog
   [this time msg]
   (update-in this [:logs] conj (LogEntry. time msg)))
  (formatLogs
   [this]
   (apply str (map #(format "Time: %d, msg: %s\n" (:time %) (:msg %)) (:logs this)))))