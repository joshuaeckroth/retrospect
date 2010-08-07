(ns simulator.types.results
  (:use [simulator.types.states :only (get-events)]))

(defmulti result-headers :Problem)

(defprotocol ResultsOperations
  (add-result [this result])
  (get-data [this]))

(defrecord Results [r]
  ResultsOperations
  (add-result [this result] (update-in this [:r] conj result))
  (get-data [this] (map #(concat [(:time %) (:percent %) (:sensor-coverage %)] (:params %)) r)))

(defprotocol SingleResultOperations
  (get-true-log [this])
  (get-true-events [this])
  (get-strat-log [this])
  (get-strat-events [this]))

(defrecord Result [time percent sensor-coverage truestate strat-state params]
  SingleResultOperations
  (get-true-log [this] (:logs truestate))
  (get-true-events [this] (get-events truestate))
  (get-strat-log [this] (:logs strat-state))
  (get-strat-events [this] (get-events strat-state)))

