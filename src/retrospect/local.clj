(ns retrospect.local
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:use [clojure.stacktrace :only [print-cause-trace]])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problem :only (run get-comparative-headers get-headers)])
  (:use [retrospect.random]))

(def write-agent (agent []))

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
  [all-results filename problem meta? results]
  (let [more-all-results (sort-by :Seed (conj all-results results)) 
        headers (if meta? (get-comparative-headers problem) (get-headers problem))]
    (with-open [writer (io/writer filename)]
      (.write writer (format-csv-row (map name headers)))
      (doseq [row (map (fn [r] (map (fn [h] (h r)) headers)) more-all-results)]
        (.write writer (format-csv-row row))))
    more-all-results))

(defn run-partition
  [problem monitor? meta? filename params]
  (when (not-empty params)
    (let [results (run problem monitor? meta? (first params))]
      (send-off write-agent write-csv filename problem meta? results)
      (recur problem monitor? meta? filename (rest params)))))

(defn check-progress
  [remaining dir problem total start-time]
  (when (> remaining 0)
    (let [progress (count @write-agent)]
      (when (< 0 progress)
        (print-progress (- (.getTime (Date.)) start-time) progress total))
      (. Thread (sleep 30000))
      (send *agent* #'check-progress dir problem total start-time)
      (- total progress))))

(defn run-partitions
  [dir problem filename params nthreads monitor? meta? repetitions]
  (send (agent (* repetitions (count params)))
        check-progress dir problem (* repetitions (count params)) (.getTime (Date.)))
  (let [seeded-params (for [p params i (range repetitions)]
                        (assoc p :Seed (my-rand-int 10000000)))
        partitions (partition-all (math/ceil (/ (count seeded-params) nthreads))
                                  (my-shuffle seeded-params))
        workers (doall (for [part partitions]
                         (future (run-partition problem monitor? meta?
                                                filename part))))]
    (doall (pmap (fn [w] @w) workers))))

(defn run-local
  [problem params dir nthreads monitor? meta? repetitions]
  (run-partitions dir problem (str dir "/results.csv")
                  params nthreads monitor? meta? repetitions))
