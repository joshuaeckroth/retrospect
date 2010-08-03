(ns simulator.types.results)

(defprotocol ResultsOperations
  (addResult [this result])
  (getResults [this]))

(defrecord Results [r]
  ResultsOperations
  (addResult [this result] (update-in this [:r] conj result))
  (getResults [this] (:r this)))

