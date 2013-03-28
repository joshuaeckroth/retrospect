(ns retrospect.local
  (:import (java.util Date))
  (:use [clojure.java.io :as io :only (writer file)])
  (:require [clojure.string :as str])
  (:use [retrospect.simulate :only [run]])
  (:use [granary.misc])
  (:use [retrospect.random])
  (:use [clojure-csv.core :only [write-csv]])
  (:use [retrospect.profile :only [profile]])
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
    (println (format "\nDone %d/%d;\t Elapsed: %s;\t Remaining: %s;\t Ending %s\n"
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

(defn run-partition
  [comparative? recdir params start-time sim-count save-record?]
  (loop [ps params]
    (when (< 0 @progress)
      (print-progress (- (.getTime (Date.)) start-time) @progress sim-count))
    (if (not-empty ps)
      (if comparative?
        (let [[control-results comparison-results comparative-results]
              (run comparative? (first ps))]
          (when save-record?
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
                                 rs)))
          (dosync
           (alter progress inc))
          (recur (rest ps)))
        (let [control-results (run comparative? (first ps))]
          (when save-record?
            (doseq [rs control-results]
              (write-results-csv (format "%s/control-results-%d.csv"
                                         recdir (:simulation (first ps))) rs)))
          (dosync (alter progress inc))
          (recur (rest ps)))))))

(defn run-partitions
  [run-meta comparative? params recdir nthreads save-record? repetitions]
  (when save-record? (spit (format "%s/meta.clj" recdir) (pr-str run-meta)))
  (let [start-time (.getTime (Date.))
        sim-count (* repetitions (count (set params)))
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
        partitions (partition-all (int (Math/ceil (/ (count numbered-params) nthreads)))
                                  (my-shuffle numbered-params))
        workers (for [part partitions]
                  (future (run-partition comparative? recdir part
                                         start-time sim-count save-record?)))]
    (profile (doall (pmap (fn [w] @w) workers)))
    (let [run-meta-stopped (assoc run-meta :endtime
                                  (format-date-ms (. System (currentTimeMillis))))]
      (when save-record? (spit (format "%s/meta.clj" recdir) (pr-str run-meta-stopped))))))
