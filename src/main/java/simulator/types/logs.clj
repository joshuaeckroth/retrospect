(ns simulator.types.logs
  (:use [simulator.types.hypotheses :only (get-hyp-id-str)]))

(defrecord LogEntry [time msg]
  Object
  (toString [_] (format "At %d, %s" time msg)))

(defrecord AbducerLogEntry [time hyps msg]
  Object
  (toString [_] (format "At %d, hypotheses: %s\n\t%s"
			time (apply str (interpose "," (map :id hyps))) msg)))

(defrecord HypLogEntry [time hyp msg]
  Object
  (toString [_] (format "At %d, hyp %s, %s" time (get-hyp-id-str hyp) msg)))
