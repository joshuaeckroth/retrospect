(ns retrospect.utility
  (:use [retrospect.profile :only [prof]]))

(defn conjs
  [coll x]
  (prof :conjs (conj (set coll) x)))
