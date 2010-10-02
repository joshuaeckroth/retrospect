(ns simulator.problems.tracking.repl
  (:use simulator.repl)
  (:use [simulator.problems.tracking.eventlog :only (get-events get-entities)]))

(def *trueevents*)

(defn update-trueevents
  [te]
  (def *trueevents* te))

(defn trueevents-at
  [time]
  (doseq [e (filter #(= (:time %) time) (get-events *trueevents*))]
    (println (str e))))

(defn trueentities-at
  [time]
  (doseq [e (filter #(= (:time (last (:snapshots %))) time) (get-entities *trueevents*))]
    (println (str e))))

(defn everything-at
  [time]
  (println (format "=== Entities at %d         ===" time))
  (trueentities-at time)
  (println (format "=== Events at %d           ===" time))
  (trueevents-at time)
  (println (format "=== Unexplained before %d  ===" time))
  (unexplained-before time)
  (println (format "=== Hypothesized at %d     ===" time))
  (hypothesized-at time)
  (println (format "=== Strategy log at %d     ===" time))
  (log-at time)
  (println (format "=== Abducer log at %d      ===" time))
  (abducer-log-at time)
  (println (format "=== Accepted at %d         ===" time))
  (accepted-at time)
  (println (format "=== Unexplained after %d   ===" time))
  (unexplained-after time))
