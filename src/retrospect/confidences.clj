(ns retrospect.confidences)

(defn confidence-str
  [conf]
  (if conf (format "%.2f" conf) "?"))

(defn penalize
  [conf]
  (* 0.8 conf)) 

(defn boost
  [conf]
  (* 1.2 conf))


