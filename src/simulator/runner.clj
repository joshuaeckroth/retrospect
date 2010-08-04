(ns simulator.runner
  (:require [simulator.types results])
  (:import [simulator.types.results Results])
  (:use [simulator.types.results :only (ResultsOperations addResult getResults)])
  (:use [incanter.io :only (read-dataset)])
  (:import (java.io BufferedWriter FileWriter)))

(defn parallel-runs
  [params runner]
  (apply concat (pmap (fn [partition] (map #(apply runner %) partition))
		      (partition-all 100 (shuffle params)))))

(defn multiple-runs [params runner]
  (let [results (parallel-runs params runner)]
    (reduce (fn [m r] (addResult m r)) (Results. []) results)))

(defn write-csv
  [filename data]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (doseq [row data] (.write writer (apply str (concat (interpose "," row) [\newline]))))))

(defn save-results
  [filename headers results]
  (write-csv filename (concat headers (getResults results))))

(defn read-results [filename]
  (read-dataset filename :header true))
