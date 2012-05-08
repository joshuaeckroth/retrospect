(ns retrospect.local
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:use [clojure.stacktrace :only [print-cause-trace]])
  (:require [clojure.string :as str])
  (:require [clojure.contrib.math :as math])
  (:use [retrospect.simulate :only [run]])
  (:use [retrospect.random])
  (:require [retrospect.database :as db])
  (:use [clojure-csv.core :only [write-csv]])
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

(defn write-results-csv
  [filename results]
  (let [new-file? (not (. (io/file filename) exists))
        row (map (fn [field] (get results field))
                 (sort (keys results)))]
    (with-open [writer (io/writer filename :append true)]
      (when new-file?
        (.write writer (write-csv [(map name (sort (keys results)))])))
      (.write writer (write-csv [(map str row)])))))

(def local-results (ref []))

(defn run-partition
  [comparative? recdir params start-time sim-count]
  (loop [ps params]
    (when (< 0 @progress)
      (print-progress (- (.getTime (Date.)) start-time) @progress sim-count))
    (if (not-empty ps)
      (if comparative?
        (let [[control-results comparison-results comparative-results]
              (run comparative? (first ps))]
          (doseq [rs control-results]
            (write-results-csv (format "%s/control-results-%d.csv"
                                       recdir (:simulation (ffirst ps)))
                               rs))
          (doseq [rs comparison-results]
            (write-results-csv (format "%s/comparison-results-%d.csv"
                                       recdir (:simulation (ffirst ps)))
                               rs))
          (doseq [rs comparative-results]
            (write-results-csv (format "%s/comparative-results-%d.csv"
                                       recdir (:simulation (ffirst ps)))
                               rs))
          (dosync
           (alter progress inc)
           (alter local-results conj {:control control-results
                                      :comparison comparison-results
                                      :comparative comparative-results}))
          (recur (rest ps)))
        (let [control-results (run comparative? (first ps))]
          (doseq [rs control-results]
            (write-results-csv (format "%s/control-results-%d.csv"
                                       recdir (:simulation (first ps))) rs))
          (dosync (alter progress inc)
                  (alter local-results conj {:control control-results}))
          (recur (rest ps)))))))

(defn run-partitions
  [run-meta comparative? params recdir nthreads upload? repetitions]
  (let [start-time (.getTime (Date.))
        sim-count (* repetitions (count params))
        seeds (repeatedly repetitions #(my-rand-int 10000000))
        seeded-params (mapcat (fn [pp] (for [s seeds]
                                         (if comparative?
                                           (map (fn [p] (assoc p :Seed s)) pp)
                                           (assoc pp :Seed s))))
                              params)
        numbered-params (map (fn [i]
                               (if comparative?
                                 (map #(assoc % :simulation i) (nth seeded-params i))
                                 (assoc (nth seeded-params i) :simulation i)))
                             (range (count seeded-params)))
        partitions (partition-all (math/ceil (/ (count numbered-params) nthreads))
                                  (my-shuffle numbered-params))
        workers (for [part partitions]
                  (future (run-partition comparative? recdir part start-time sim-count)))]
    (doall (pmap (fn [w] @w) workers))
    (let [run-meta-stopped (assoc run-meta :endtime (. System (currentTimeMillis)))]
      (spit (format "%s/meta.clj" recdir) (pr-str run-meta-stopped))
      (when (and upload? (not= "" @database))
        (println "Writing results to database...")
        (db/commit-run run-meta-stopped @local-results)))))
