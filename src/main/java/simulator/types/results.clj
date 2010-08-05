(ns simulator.types.results)

(defprotocol ResultsOperations
  (addResult [this result])
  (getData [this]))

(defrecord Results [r]
  ResultsOperations
  (addResult [this result] (update-in this [:r] conj result))
  (getData [this] (map #(concat [(:time %) (:percent %)] (:params %)) (:r this))))

(defrecord Result [time percent params])

