(ns simulator.types.logs)

(defrecord LogEntry [time msg]
  Object
  (toString [_] (format "At %d, %s" time msg)))

(defrecord AbducerLogEntry [time hyps msg]
  Object
  (toString [_] (format "At %d, hypotheses: %s\n\t%s"
			time (apply str (interpose "," (map :id hyps))) msg)))
