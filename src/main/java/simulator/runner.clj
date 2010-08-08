(ns simulator.runner
  (:require [simulator.types results])
  (:import [simulator.types.results Results Result])
  (:use [simulator.types.results :only (ResultsOperations add-result get-data)])
  (:use [incanter.io :only (read-dataset)])
  (:import (java.io BufferedWriter FileWriter))
  (:import (java.util Date)))

(defn format-time
  [seconds]
  (let [hours (int (/ seconds 3600.0))
	mins (int (/ (rem seconds 3600) 60.0))
	secs (rem seconds 60)]
    (format "%dh %dm %ds" hours mins secs)))

(defn print-progress
  [time milliseconds finished total]
  (let [remaining (- total finished)
	avgtime (/ time finished)
	expected (* remaining avgtime)
	wallexpected (.toString (Date. (long (+ expected (.getTime (Date.))))))]
    (println (format "Done %d/%d; Elapsed: %s; Remaining: %s, ending %s"
		     finished
		     total
		     (format-time (int (/ time 1000.0)))
		     (format-time (int (/ expected 1000.0)))
		     wallexpected))))

(defn average-runs
  [runner p n]
  (let [runs (for [i (range n)] (apply runner p))
	avg (fn [field rs] (double (/ (reduce + (map field rs)) n)))]
    (Result. (avg :time runs) (avg :percent runs) (avg :sensor-coverage runs)
	     nil nil (take 6 p))))

(defn run-partition
  [runner partition]
  (doall (map #(average-runs runner % 10) partition)))

(defn parallel-runs
  [params runner nthreads]
  "Each thread will have 10 configurations (and average 10 over runs per configuration)."
  (let [partitions (partition-all 10 (shuffle params))
	total (* 10 (count partitions))]
    (loop [pofps (partition-all nthreads partitions)
	   finished 0
	   time 0.0
	   results []]
      (if pofps
	(do
	  (let [ps (first pofps)
		starttime (. System (nanoTime))
		rs (doall (pmap #(run-partition runner %) ps))
		endtime (. System (nanoTime))
		milliseconds (/ (- endtime starttime) 1000000.0)
		newtime (+ time milliseconds)
		newfinished (+ (* 10 (count ps)) finished)]
	    (print-progress newtime milliseconds newfinished total)
	    (recur (next pofps) newfinished newtime (reduce concat results rs))))
	results))))

(defn multiple-runs [params runner nthreads]
  (let [results (parallel-runs params runner nthreads)]
    (reduce (fn [m r] (add-result m r)) (Results. []) results)))

(defn write-csv
  [filename data]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (doseq [row data] (.write writer (apply str (concat (interpose "," row) [\newline]))))))

(defn save-results
  [filename headers results]
  (write-csv filename (concat [(concat ["Milliseconds" "PercentCorrect" "SensorCoverage"] headers)]
			      (get-data results))))

