(ns samre.problems.tracking.repl
  (:use samre.repl))

(def *truedata*)

(defn update-truedata
  [td]
  (def *truedata* td))

