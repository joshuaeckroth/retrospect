(ns samre.runners.local
  (:use [incanter.io :only (read-dataset)])
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:use [clojure.stacktrace :only [print-cause-trace]])
  (:use [samre.problem :only (average-runs get-headers)])
  (:use [samre.charts :only (save-plots)]))

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
    (println (format "Done %d/%d;\t Elapsed: %s;\t Remaining: %s;\t Ending %s"
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
  [problem monitor filename params]
  (when (not-empty params)
    (send-off write-agent write-csv filename problem
              (average-runs problem monitor (first params) 10))
    (recur problem monitor filename (rest params))))

(defn check-progress
  [remaining dir problem total start-time]
  (when (> remaining 0)
    (let [progress @write-agent]
      (when (< 0 progress)
        (print-progress (- (.getTime (Date.)) start-time) progress total)
        (copy (file (str dir "/results.csv")) (file (str dir "/results-copy.csv")))
        (save-plots dir problem))
      (. Thread (sleep 30000))
      (send *agent* #'check-progress dir problem total start-time)
      (- total progress))))

(defn run-partitions
  [dir problem filename params nthreads monitor]
  (with-open [writer (io/writer filename)]
    (.write writer (format-csv-row (map name (get-headers problem)))))
  (send (agent (count params)) check-progress dir problem (count params) (.getTime (Date.)))
  (let [partitions (partition (int (/ (count params) nthreads)) (shuffle params))]
    (doall (pmap (partial run-partition problem monitor filename) partitions))))

(defn run-local
  [problem params dir nthreads monitor]
  (run-partitions dir problem (str dir "/results.csv") params nthreads monitor)
  #_(try

    (catch Exception e
      (print-cause-trace e)
      (println "Quitting early.")
      ;; indicate maximum progress so check-progress agent stops
      (send-off write-agent (constantly (count params))))))
