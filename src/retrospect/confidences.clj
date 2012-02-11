(ns retrospect.confidences)

(defn conf-str
  [conf]
  (if conf (format "%.2f" conf) "?"))

(defn penalize
  [conf]
  (* 0.8 conf)) 

(defn boost
  [conf]
  (* 1.2 conf))


