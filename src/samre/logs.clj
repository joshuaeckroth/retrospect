(ns samre.logs)

(defrecord LogEntry [msg]
  Object
  (toString [_] (format "%s" msg)))

(defrecord AbducerLogEntry [hyp-pids msg]
  Object
  (toString [_] (format "Hypotheses: %s\n\t%s"
			(apply str (interpose ", " hyp-pids)) msg)))

(defrecord MetaLogEntry [ep-state ep-state-revisit msg]
  Object
  (toString [_] (format "Meta-abduction (%s -> %s): %s"
                        (str ep-state) (str ep-state-revisit) msg)))
