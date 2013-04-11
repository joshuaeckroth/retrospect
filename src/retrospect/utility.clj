(ns retrospect.utility
  (:use [retrospect.profile :only [prof]]))

;; from clojure.contrib.core
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (prof :dissoc-in
        (if ks
          (if-let [nextmap (get m k)]
            (let [newmap (dissoc-in nextmap ks)]
              (if (seq newmap)
                (assoc m k newmap)
                (dissoc m k)))
            m)
          (dissoc m k))))

(defn conjs
  [coll x]
  (prof :conjs (conj (set coll) x)))
