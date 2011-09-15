(ns retrospect.local
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:use [clojure.stacktrace :only [print-cause-trace]])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problem :only [run]])
  (:use [retrospect.random])
  (:require [retrospect.database :as db]))

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

;; keep track of progress
(def progress (ref 0))

(defn check-progress
  [remaining total start-time]
  (when (> remaining 0)
    (let [p @progress]
      (when (< 0 p)
        (print-progress (- (.getTime (Date.)) start-time) p total))
      (. Thread (sleep 30000))
      (send *agent* #'check-progress total start-time)
      (- total p))))

(defn format-csv-row
  [row]
  ;; add quotes around string data (e.g. "meta,trans,lazy")
  (apply str (concat (interpose "," (map #(if (= String (type %)) (format "\"%s\"" %) %) row))
                     [\newline])))

(defn write-csv
  [results-type filename problem results]
  (let [new-file? (not (. (io/file filename) exists))]
    (with-open [writer (io/writer filename :append true)]
      (when new-file?
        (.write writer (format-csv-row (map name (sort (keys results))))))
      (.write writer (format-csv-row (map (fn [field] (get results field))
                                          (sort (keys results))))))))

(defn run-partition
  [problem monitor? recorddir datadir params]
  (when (not-empty params)
    (let [[control-results comparison-results comparative-results]
          (run problem monitor? datadir (first params))]
      (db/put-results-row :control control-results)
      (db/put-results-row :comparative comparative-results)
      (db/put-results-row :comparison comparison-results)
      (write-csv :control (str recorddir "/control-results.csv")
                 problem control-results)
      (write-csv :comparison (str recorddir "/comparison-results.csv")
                 problem comparison-results)
      (write-csv :comparative (str recorddir "/comparative-results.csv")
                 problem comparative-results)
      (dosync (alter progress inc))
      (recur problem monitor? recorddir datadir (rest params)))))

(defn run-partitions
  [problem params recorddir datadir nthreads monitor? repetitions]
  (let [sim-count (* repetitions (count params))]
    (send (agent sim-count) check-progress sim-count (.getTime (Date.)))) 
  (let [seeded-params (for [p params i (range repetitions)]
                        (assoc p :Seed (my-rand-int 10000000)))
        partitions (partition-all (math/ceil (/ (count seeded-params) nthreads))
                                  (my-shuffle seeded-params))
        workers (doall (for [part partitions]
                         (future (run-partition problem monitor? recorddir datadir part))))]
    (doall (pmap (fn [w] @w) workers))))

