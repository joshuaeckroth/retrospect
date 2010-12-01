(ns simulator.runners.local
  (:use [incanter.io :only (read-dataset)])
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer)])
  (:use [simulator.problem :only (average-strategies get-headers)]))

(def write-agent (agent 0))

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
  [progress filename problem results]
  (with-open [writer (io/writer filename :append true)]
    (doseq [row (map (fn [r] (map (fn [h] (h r)) (get-headers problem))) results)]
      (.write writer (format-csv-row row))))
  (inc progress))

(defn run-partition
  [problem filename params]
  (when (not-empty params)
    (send-off write-agent write-csv filename problem
              (average-strategies problem (first params) 1))
    (recur problem filename (rest params))))

(defn check-progress
  [remaining total start-time]
  (when (> remaining 0)
    (let [progress @write-agent]
      (if (< 0 progress)
        (print-progress (- (.getTime (Date.)) start-time) progress total))
      (. Thread (sleep 30000))
      (send *agent* #'check-progress total start-time)
      (- total progress))))

(defn run-partitions
  [problem filename params nthreads]
  (with-open [writer (io/writer filename)]
    (.write writer (format-csv-row (map name (get-headers problem)))))
  (send (agent (count params)) check-progress (count params) (.getTime (Date.)))
  (let [partitions (partition (int (/ (count params) nthreads)) (shuffle params))]
    (doall (pmap (partial run-partition problem filename) partitions))))

(defn run-local
  [problem params dir nthreads]
  (run-partitions problem (str dir "/results.csv") params nthreads))
