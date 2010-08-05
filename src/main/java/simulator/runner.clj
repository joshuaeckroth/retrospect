(ns simulator.runner
  (:require [simulator.types results])
  (:import [simulator.types.results Results Result])
  (:use [simulator.types.results :only (ResultsOperations addResult getData)])
  (:use [incanter.io :only (read-dataset)])
  (:import (java.io BufferedWriter FileWriter)))

(defn average-runs
  [runner p n]
  (let [runs (for [i (range n)] (apply runner p))
	avg (fn [field rs] (/ (reduce + (map field rs)) 10.0))]
    (Result. (avg :time runs) (avg :percent runs) nil nil p)))

(defn run-partition
  [runner partition]
  (println (format "Running %d configurations (averaging 10 runs per configuration)..." (count partition)))
  (doall (map #(average-runs runner % 10) partition))
  (println "Thread done."))

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
  (write-csv filename (concat (concat ["Milliseconds" "PercentCorrect"] headers) (getData results))))

(defn read-results [filename]
  (read-dataset filename :header true))
