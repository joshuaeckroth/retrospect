(ns simulator.types.logs)

(defrecord LogEntry [time msg]
  Object
  (toString [_] (format "At %d, %s" time msg)))

(defrecord AbducerLogEntry [time hyp-ids msg]
  Object
  (toString [_] (format "At %d, hypotheses: %s\n\t%s"
			time (apply str (interpose "," hyp-ids)) msg)))

(defrecord HypLogEntry [time hyp-id msg]
  Object
  (toString [_] (format "At %d, hyp %s, %s" time hyp-id msg)))
