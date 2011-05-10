(ns retrospect.confidences
  (:use [clojure.contrib.math :only [round]]))

(def IMPOSSIBLE -3)

(def VERY-IMPLAUSIBLE -2)

(def IMPLAUSIBLE -1)

(def NEUTRAL 0)

(def PLAUSIBLE 1)

(def VERY-PLAUSIBLE 2)

(defn confidence-str
  [conf]
  (if conf
    (case conf
          -3 "IM"
          -2 "VI"
          -1 "I"
          0 "N"
          1 "P"
          2 "VP")
    "?"))

(defn penalize
  [conf]
  (max VERY-IMPLAUSIBLE (dec conf)))

(defn boost
  [conf]
  (min VERY-PLAUSIBLE (inc conf)))

(defn prob-conf
  [prob]
  (cond
   (<= prob 20) VERY-IMPLAUSIBLE
   (<= prob 40) IMPLAUSIBLE
   (<= prob 60) NEUTRAL
   (<= prob 80) PLAUSIBLE
   :else VERY-PLAUSIBLE))

(defn prob-neg-conf
  [prob]
  (prob-conf (- 100 prob)))

(defn avg-conf
  [confs]
  (if (empty? confs) NEUTRAL
      (let [avg (/ (reduce + 0 confs) (count confs))]
        (round avg))))