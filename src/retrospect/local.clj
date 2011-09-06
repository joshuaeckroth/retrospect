(ns retrospect.local
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:use [clojure.stacktrace :only [print-cause-trace]])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problem :only [run]])
  (:use [retrospect.random])
  (:require [retrospect.database :as db]))

;; keep track of number of writes, so that on first write,
;; we can write the headers
(def write-agent (agent {:control 0 :comparison 0 :comparative 0}))

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
  ;; add quotes around string data (e.g. "meta,trans,lazy")
  (apply str (concat (interpose "," (map #(if (= String (type %)) (format "\"%s\"" %) %) row))
                     [\newline])))

(defn write-csv
  [numwrites results-type filename problem results]
  (db/put-results-row results-type results)
  (with-open [writer (io/writer filename :append true)]
    (when (= 0 (results-type numwrites))
      (.write writer (format-csv-row (map name (sort (keys results))))))
    (.write writer (format-csv-row (map (fn [field] (get results field))
                                        (sort (keys results))))))
  (update-in numwrites [results-type] inc))

(defn run-partition
  [problem monitor? recorddir datadir params]
  (when (not-empty params)
    (let [[control-results comparison-results comparative-results]
          (run problem monitor? datadir (first params))]
      (send-off write-agent write-csv :control (str recorddir "/control-results.csv")
                problem control-results)
      (send-off write-agent write-csv :comparison (str recorddir "/comparison-results.csv")
                problem comparison-results)
      (send-off write-agent write-csv :comparative (str recorddir "/comparative-results.csv")
                problem comparative-results)
      (recur problem monitor? recorddir datadir (rest params)))))

(defn check-progress
  [remaining total start-time]
  (println @write-agent)
  (when (> remaining 0)
    (let [progress (count (:control @write-agent))]
      (println progress)
      (when (< 0 progress)
        (print-progress (- (.getTime (Date.)) start-time) progress total))
      (. Thread (sleep 30000))
      (send *agent* #'check-progress total start-time)
      (- total progress))))

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

