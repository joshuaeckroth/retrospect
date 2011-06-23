(ns retrospect.confidences)

(defn confidence-str
  [conf]
  (format "%.2f" conf))

(defn penalize
  [conf]
  (* 1.2 conf)) 

(defn boost
  [conf]
  (* 0.8 conf))


