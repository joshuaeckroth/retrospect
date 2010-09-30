(ns simulator.runners.local
  (:use [incanter.io :only (read-dataset)])
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer)])
  (:use [simulator.types.problem :only (average-strategy-runs get-headers)]))

(def progress (agent 0))

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

(defn format-csv-row
  [row]
  (apply str (concat (interpose "," row) [\newline])))

(defn write-csv
  [filename headers results]
  (with-open [writer (io/writer filename)]
    (.write writer (format-csv-row (map name headers)))
    (doseq [row (map (fn [r] (map (fn [h] (h r)) headers)) results)]
      (.write writer (format-csv-row row)))))

(defn run-partition
  [x problem params]
  (loop [ps params
         results []]
    (if (not-empty ps)
      (let [rs (doall (average-strategy-runs problem (first ps) 2))]
        (send progress inc)
        (recur (rest ps) (doall (concat results rs))))
      results)))

(defn check-progress
  [remaining total]
  (when (> remaining 0)
    (let [progress @progress]
      (println (format "Complete: %d/%d, remaining: %d"
                       progress total (- total progress)))
      (. Thread (sleep 10000))
      (send *agent* #'check-progress total)
      (- total progress))))

(defn run-agents
  [problem params nthreads]
  (send (agent (count params)) check-progress (count params))
  (let [partitions (partition (int (/ (count params) nthreads)) (shuffle params))
        agents (take (count partitions) (repeatedly #(agent nil)))]
    (doseq [i (range (count agents))]
      (send (nth agents i) run-partition problem (nth partitions i)))
    (apply await agents)
    (apply concat (map deref agents))))

(defn run-local
  [problem params dir nthreads]
  (let [results (run-agents problem params nthreads)]
    (write-csv (str dir "/results.csv") (get-headers problem) results)))
