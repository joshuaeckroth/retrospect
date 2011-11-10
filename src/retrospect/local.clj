(ns retrospect.local
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:use [clojure.stacktrace :only [print-cause-trace]])
  (:require [clojure.string :as str])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.problem :only [run]])
  (:use [retrospect.random])
  (:require [retrospect.database :as db])
  (:use [retrospect.state]))

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
  ;; add quotes around string data
  (let [fmt (fn [s] (format "\"%s\"" (str/replace s "\"" "\\\"")))]
    (apply str (concat (interpose "," (map #(if (= String (type %)) (fmt %) %) row))
                       [\newline]))))

(defn write-csv
  [filename results]
  (let [new-file? (not (. (io/file filename) exists))
        row (map (fn [field] (get results field))
                 (sort (keys results)))]
    (with-open [writer (io/writer filename :append true)]
      (when new-file?
        (.write writer (format-csv-row (map name (sort (keys results))))))
      (.write writer (format-csv-row row)))))

(def results (ref []))

(defn run-partition
  [comparative? monitor? recdir params]
  (loop [ps params]
    (if (not-empty ps)
      (if comparative?
        (let [[control-results comparison-results comparative-results]
              (run comparative? monitor? (first ps))]
          (doseq [rs control-results]
            (write-csv (str recdir "/control-results.csv") rs))
          (doseq [rs comparison-results]
            (write-csv (str recdir "/comparison-results.csv") rs))
          (doseq [rs comparative-results]
            (write-csv (str recdir "/comparative-results.csv") rs))
          (dosync
           (alter progress inc)
           (alter results conj {:control control-results
                                :comparison comparison-results
                                :comparative comparative-results}))
          (recur (rest ps)))
        (let [control-results (run comparative? monitor? (first ps))]
          (doseq [rs control-results]
            (write-csv (str recdir "/control-results.csv") rs))
          (dosync (alter progress inc)
                  (alter results conj {:control control-results}))
          (recur (rest ps)))))))

(defn run-partitions
  [run-meta comparative? params recdir nthreads monitor? repetitions]
  (let [sim-count (* repetitions (count params))]
    (send (agent sim-count) check-progress sim-count (.getTime (Date.))))
  (let [seeds (repeatedly repetitions #(my-rand-int 10000000))
        seeded-params (mapcat (fn [pp] (for [s seeds]
                                         (if comparative?
                                           (map (fn [p] (assoc p :Seed s)) pp)
                                           (assoc pp :Seed s))))
                              params)
        partitions (partition-all (math/ceil (/ (count seeded-params) nthreads))
                                  (my-shuffle seeded-params))
        workers (for [part partitions]
                  (future (run-partition comparative? monitor? recdir part)))]
    (doall (pmap (fn [w] @w) workers))
    (when (not= "" @database)
      (println "Writing results to database...")
      (db/commit-run run-meta @results))))
