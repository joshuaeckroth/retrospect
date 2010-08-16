(ns simulator.runners.local
  (:use [incanter.io :only (read-dataset)])
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer)])
  (:use [simulator.types.problem :only (average-some-runs get-headers)]))

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

(defn parallel-runs
  [problem params nthreads]
  "Each thread will have 10 configurations (and average 10 over runs per configuration)."
  (let [n 10
	partitions (partition-all n (shuffle params))
	total (* n (count partitions))]
    (loop [pofps (partition-all nthreads partitions)
	   finished 0
	   time 0.0
	   results []]
      (if pofps
	(do
	  (let [part (first pofps)
		starttime (. System (nanoTime))
		rs (apply concat (pmap #(average-some-runs problem % n) part))
		endtime (. System (nanoTime))
		milliseconds (/ (- endtime starttime) 1000000.0)
		newtime (+ time milliseconds)
		newfinished (+ (* n (count part)) finished)]
	    (print-progress newtime milliseconds newfinished total)
	    (recur (next pofps) newfinished newtime (concat rs results))))
	results))))

(defn format-csv-row
  [row]
  (apply str (concat (interpose "," row) [\newline])))

(defn write-csv
  [filename headers results]
  (with-open [writer (io/writer filename)]
    (.write writer (format-csv-row (map name headers)))
    (doseq [row (map (fn [r] (map (fn [h] (h r)) headers)) results)]
      (.write writer (format-csv-row row)))))

(defn run-local
  [problem params dir nthreads]
  (let [results (parallel-runs problem params nthreads)]
    (write-csv (str dir "/results.csv") (get-headers problem) results)))
