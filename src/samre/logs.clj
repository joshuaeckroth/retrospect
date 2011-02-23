(ns samre.logs)

(defrecord LogEntry [msg]
  Object
  (toString [_] (format "%s" msg)))

(defrecord AbducerLogEntry [hyp-ids msg]
  Object
  (toString [_] (format "Hypotheses: %s\n\t%s"
			(apply str (interpose ", " (map str hyp-ids))) msg)))

(defrecord MetaLogEntry [ep-state ep-state-new abducer-log]
  Object
  (toString [_] (format "Meta-abduction (%s -> %s):\n%s"
                        (str ep-state) (str ep-state-new)
                        (apply str (interpose "\n" (map str abducer-log))))))
