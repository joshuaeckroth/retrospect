(ns retrospect.utility)

(defn conjs
  [coll x]
  (conj (set coll) x))
