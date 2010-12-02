(ns simulator.confidences)

(def VERY-IMPLAUSIBLE -2)

(def IMPLAUSIBLE -1)

(def NEUTRAL 0)

(def PLAUSIBLE 1)

(def VERY-PLAUSIBLE 2)

(defn confidence-str
  [conf]
  (if conf
    (case conf
          -2 "VI"
          -1 "I"
          0 "N"
          1 "P"
          2 "VP")
    "?"))