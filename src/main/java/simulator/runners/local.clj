(ns simulator.runners.local
  (:use [incanter.io :only (read-dataset)])
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer)])
  (:use [simulator.types.problem :only (average-strategy-runs get-headers)]))

(def *progress* 0)

(defn format-time
  [seconds]
  (let [hours (int (/ seconds 3600.0))
	mins (int (/ (rem seconds 3600) 60.0))
	secs (rem seconds 60)]
    (format "%dh %dm %ds" hours mins secs)))

(defn print-progress
  [elapsed finished total]
  (let [remaining (- total finished)
	avgtime (/ elapsed finished)
	expected (* remaining avgtime)
	wallexpected (.toString (Date. (long (+ expected (.getTime (Date.))))))]
    (println (format "Done %d/%d; Elapsed: %s; Remaining: %s, ending %s"
		     finished
		     total
		     (format-time (int (/ elapsed 1000.0)))
		     (format-time (int (/ expected 1000.0)))
		     wallexpected))))

(defn format-csv-row
  [row]
  (apply str (concat (interpose "," row) [\newline])))

(defn write-csv
  [filename headers results]
  (with-open [writer (io/writer filename)]
    (.write writer (format-csv-row (map name headers)))
    (doseq [row (map (fn [r] (map (fn [h] (h r)) headers)) results)]
      (.write writer (format-csv-row row)))))

(defn check-progress
  [remaining total start-time]
  (when (> remaining 0)
    (let [progress *progress*]
      (if (< 0 progress)
        (print-progress (- (.getTime (Date.)) start-time) progress total))
      (. Thread (sleep 30000))
      (send *agent* #'check-progress total start-time)
      (- total progress))))

(defn run-partition
  [problem params]
  (loop [ps params
         results []]
    (if (not-empty ps)
      (let [rs (doall (average-strategy-runs problem (first ps) 5))]
        (def *progress* (inc *progress*)) ;; chance of race-condition; non-atomic update
        (recur (rest ps) (doall (concat results rs))))
      results)))

(defn run-partitions
  [problem params nthreads]
  (send (agent (count params)) check-progress (count params) (.getTime (Date.)))
  (let [partitions (partition (int (/ (count params) nthreads)) (shuffle params))]
    (apply concat (pmap (partial run-partition problem) partitions))))

(defn run-local
  [problem params dir nthreads]
  (let [results (run-partitions problem params nthreads)]
    (write-csv (str dir "/results.csv") (get-headers problem) results)))
