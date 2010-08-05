(ns simulator.types.results)

(defprotocol ResultsOperations
  (addResult [this result])
  (getData [this]))

(defrecord Results [r]
  ResultsOperations
  (addResult [this result] (update-in this [:r] conj result))
  (getData [this] (map #(concat [(:time %) (:percent %)] (:params %)) (:r this))))

(defprotocol SingleResultOperations
  (getTrueLog [this])
  (getTrueEvents [this])
  (getStratLog [this])
  (getStratEvents [this]))

(defrecord Result [time percent truestate strat-state params]
  SingleResultOperations
  (getTrueLog [this] (:logs truestate))
  (getTrueEvents [this] (:events truestate))
  (getStratLog [this] (:logs strat-state))
  (getStratEvents [this] (:events strat-state)))

