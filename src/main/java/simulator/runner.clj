(ns simulator.runner
  (:require [simulator.types results])
  (:import [simulator.types.results Results Result])
  (:use [simulator.types.results :only (ResultsOperations addResult getData)])
  (:use [incanter.io :only (read-dataset)])
  (:import (java.io BufferedWriter FileWriter)))

(defn average-runs
  [runner p n]
  (let [runs (for [i (range n)] (apply runner p))
	avg (fn [field rs] (double (/ (reduce + (map field rs)) n)))]
    (Result. (avg :time runs) (avg :percent runs) (avg :sensor-coverage runs) nil nil (take 6 p))))

(defn run-partition
  [runner partition]
  (println (format "Running %d configurations (averaging 10 runs per configuration)..."
		   (count partition)))
  (let [results (doall (map #(average-runs runner % 10) partition))]
    (println "Thread done.")
    results))

(defn parallel-runs
  [params runner]
  (apply concat (pmap #(run-partition runner %) (partition-all 100 (shuffle params)))))

(defn multiple-runs [params runner]
  (let [results (parallel-runs params runner)]
    (reduce (fn [m r] (addResult m r)) (Results. []) results)))

(defn write-csv
  [filename data]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (doseq [row data] (.write writer (apply str (concat (interpose "," row) [\newline]))))))

(defn save-results
  [filename headers results]
  (write-csv filename (concat [(concat ["Milliseconds" "PercentCorrect" "SensorCoverage"] headers)]
			      (getData results))))

