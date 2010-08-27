(ns simulator.types.logs)

(defrecord LogEntry [time msg]
  Object
  (toString [_] (format "At %d, %s\n" time msg)))