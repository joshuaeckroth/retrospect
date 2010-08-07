(ns simulator.types.logs
  (:use [simulator.types.generic :only (Printable to-str)]))

(defrecord LogEntry [time msg]
  Printable
  (to-str [this] (format "At %d, %s\n" time msg)))