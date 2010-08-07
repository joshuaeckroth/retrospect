(ns simulator.strategies
  (:require [simulator.types states logs])
  (:import [simulator.types.states State])
  (:import [simulator.types.logs LogEntry])
  (:use [simulator.types.entities :only (pos)])
  (:use [simulator.types.states :as states :only
	 (StateMethods add-entity update-entity add-event-new add-event-move
		       add-event get-entities get-events)]))

(defprotocol StrategyStateMethods
  (add-log [this time msg])
  (format-logs [this])
  (add-mem [this amount])
  (add-time [this amount])
  (add-real-time [this amount]))

(defrecord StrategyState [strategy state logs resources]
  states/StateMethods
  (add-entity
   [this entity]
   (assoc this :state (states/add-entity state entity)))
  (update-entity
   [this entity pos]
   (assoc this :state (states/update-entity state entity pos)))
  (add-event-new
   [this time pos]
   (assoc this :state (states/add-event-new state time pos)))
  (add-event-move
   [this time oldpos newpos]
   (assoc this :state (states/add-event-move state time oldpos newpos)))
  (add-event
   [this event]
   (assoc this :state (states/add-event state event)))
  (get-entities
   [this]
   (states/get-entities state))
  (get-events
   [this]
   (states/get-events state))
  StrategyStateMethods
  (add-log
   [this time msg]
   (update-in this [:logs] conj (LogEntry. time msg)))
  (format-logs
   [this]
   (apply str (map #(format "Time: %d, msg: %s\n" (:time %) (:msg %)) logs)))
  (add-mem
   [this amount]
   (assoc this :resources (assoc resources :compute (+ (:compute resources) amount))))
  (add-time
   [this amount]
   (assoc this :resources (assoc resources :time (+ (:time resources) amount))))
  (add-real-time
   [this amount]
   (assoc this :resources (assoc resources :real-time (+ (:real-time resources) amount)))))

(defn init-strat-state
  [strategy]
  (StrategyState. strategy (State. [] []) [] {:mem 0 :time 0 :real-time 0.0}))

(defn explain-new-entity
  [strat-state spotted time]
  (-> strat-state
      (add-entity spotted)
      (add-event-new time (pos spotted))))

(defn explain-existing-entity
  [strat-state spotted entity time]
  (-> strat-state
      (update-entity entity (pos spotted))
      (add-event-move time (pos entity) (pos spotted))))