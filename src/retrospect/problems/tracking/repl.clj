(ns retrospect.problems.tracking.repl
  (:use retrospect.repl))

(def *truedata*)

(defn update-truedata
  [td]
  (def *truedata* td))

