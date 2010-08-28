(ns simulator.strategies.core
  (:require [simulator.types logs hypotheses])
  (:import [simulator.types.logs LogEntry])
  (:import [simulator.types.hypotheses HypothesisSpace]))

(defrecord StrategyState
    [strategy hypspace accepted considering rejected
     log resources problem-data])

(defn init-strat-state
  [strategy pdata]
  (StrategyState. strategy (HypothesisSpace. [] {} {})
		  [] [] [] [] {} pdata))

(defn add-log-msg
  [strat-state time msg]
  (update-in strat-state [:log] conj (LogEntry. time msg)))

(defn format-logs
  [strat-state]
  (apply str (map str (:log strat-state))))

;; (defn explain-new-entity
;;   [strat-state spotted time]
;;   (-> strat-state
;;       (add-entity spotted)
;;       (add-event-new time (pos spotted))))

;; (defn explain-existing-entity
;;   [strat-state spotted entity time]
;;   (-> strat-state
;;       (update-entity entity (pos spotted))
;;       (add-event-move time (pos entity) (pos spotted))))

(def strategies ["guess" "nearest"])
