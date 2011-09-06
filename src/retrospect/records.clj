(ns retrospect.records
  (:import [java.io File])
  (:import [java.util Date])
  (:use [clojure.contrib.prxml :only [prxml]])
  (:use [clojure.xml :as xml :only [parse tag attrs]])
  (:use [clojure.zip :as zip :only [xml-zip node children]])
  (:use [clojure.contrib.zip-filter.xml :as zf :only [xml-> text attr=]])
  (:use [clojure.contrib.shell :only [sh]])
  (:use [clojure.contrib.io :only [pwd]])
  (:use [clojure.java.io :as io :only [writer reader copy]])
  (:use [clojure.string :only [split]])
  (:use [retrospect.local :only [run-partitions]])
  (:require [retrospect.database :as db]))

(defn copy-params-file
  [destfile paramsfile]
  (io/copy (File. paramsfile) (File. destfile)))

(defn read-params
  [problem paramsfile]
  "Reads parameters from params XML file for a certain problem. Result
  is a map like {:SensorCoverage [0 10 20], :BeliefNoise [0 10 20]}"
  (let [xmltree (zip/xml-zip (xml/parse (File. paramsfile)))
	probtree (zf/xml-> xmltree :problems :problem (attr= :name (:name problem))
			     :params children)
	probmaps (apply merge (map (fn [p] {(first (zf/xml-> p tag))
                                            (first (xml-> p attrs))}) probtree))
	probtags (keys probmaps)
	get-value (fn [pm p k] (Integer/parseInt (k (p pm))))
	update-with-range (fn [pm probtag]
			    (assoc pm probtag
				   (range (get-value probmaps probtag :start)
					  (inc (get-value probmaps probtag :end))
					  (get-value probmaps probtag :step))))]
    (reduce update-with-range {} probtags)))

(defn explode-params
  "Want {:Xyz [1 2 3], :Abc [3 4]} to become [{:Xyz 1, :Abc 3}, {:Xyz 2, :Abc 4}, ...]"
  [params]
  {:pre [(not (empty? params))]}
  (if (= 1 (count params))
    (for [v (second (first params))]
      {(first (first params)) v})
    (let [p (first params)
	  deeper (explode-params (rest params))]
      (flatten (map (fn [v] (map #(assoc % (first p) v) deeper)) (second p))))))

(defn git-meta-info
  []
  (sh "/usr/bin/git" "log" "-n" "1"))

(defn run-with-new-record
  "Create a new folder for storing run data and execute the run."
  [problem control comparison paramsfile
   datadir recordsdir nthreads monitor? repetitions]
  (try
    (let [t (int (/ (. System (currentTimeMillis)) 1000))
          recorddir (str recordsdir "/" t)
          params (map #(merge {:Control control :Comparison comparison} %)
                      (explode-params (read-params problem paramsfile)))]
      (print (format "Making new directory %s..." recorddir))
      (.mkdir (File. recorddir))
      (println "done.")
      (print "Copying params file...")
      (copy-params-file (str recorddir "/params.xml") paramsfile)
      (println "done.")
      (print "Creating new database record...")
      (db/new-active {:time t :datadir datadir :recordsdir recordsdir :nthreads nthreads
                      :pwd (pwd) :monitor monitor? :repetitions repetitions
                      :hostname (.getHostName (java.net.InetAddress/getLocalHost))
                      :git (git-meta-info)})
      (println "done.")
      (println
        (format "Running %d parameters, %d repetitions = %d simulations..."
                (count params) repetitions (* (count params) repetitions)))
      (run-partitions problem params recorddir datadir nthreads monitor? repetitions)
      (println "Done."))
    (catch java.util.concurrent.ExecutionException e
      (println "Quitting early."))))



