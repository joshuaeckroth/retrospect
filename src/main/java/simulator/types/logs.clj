(ns simulator.types.logs
  (:use [simulator.types.generic :only (Printable toStr)]))

(defrecord LogEntry [time msg]
  Printable
  (toStr [this] (format "At %d, %s\n" time msg)))