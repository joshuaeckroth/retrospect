(ns retrospect.confidences)

(defn confidence-str
  [conf]
  (format "%.2f" conf))

(defn penalize
  [conf]
  (* 0.8 conf)) 

(defn boost
  [conf]
  (- 1.0 (* 0.8 (- 1.0 conf))))


