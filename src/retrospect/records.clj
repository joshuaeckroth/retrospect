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
  (:use [clojure.string :only [split-lines trim]])
  (:use [retrospect.local :only [run-partitions]])
  (:require [retrospect.database :as db])
  (:use [retrospect.state]))

(defn vectorize-params
  [params]
  (reduce (fn [m k] (let [v (k params)]
                      (assoc m k (if (vector? v) v [v]))))
          {} (keys params)))

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
  (let [[commit _ _ _ & msg] (split-lines (sh "git" "log" "-n" "1"))
        branch (trim (subs (sh "git" "branch" "--contains") 2))]
    {:commit (subs commit 7)
     :commit-msg (apply str (interpose "\n" (map (fn [s] (subs s 4)) msg)))
     :branch branch}))

(defn run-with-new-record
  "Create a new folder for storing run data and execute the run."
  [seed recordsdir nthreads monitor? repetitions]
  (try
    (let [t (. System (currentTimeMillis))
          recdir (str recordsdir "/" t)
          control-params (explode-params (vectorize-params (:control @params)))
          comparison-params (explode-params (vectorize-params (:comparison @params)))
          paired-params (partition 2 (interleave control-params comparison-params))]
      (when (not= (count control-params) (count comparison-params))
        (println "Control/comparison param counts are not equal.")
        (System/exit -1))
      (print (format "Making new directory %s..." recdir))
      (.mkdir (File. recdir))
      (println "done.")
      (print "Creating new database record...")
      (db/new-active (merge {:type "run" :time t :paramsid (:_id @params) :paramsrev (:_rev @params)
                             :paramsname (format "%s/%s" (:name @problem) (:name @params))
                             :datadir @datadir :recorddir recdir :nthreads nthreads
                             :pwd (pwd) :monitor monitor? :repetitions repetitions
                             :hostname (.getHostName (java.net.InetAddress/getLocalHost))
                             :username (System/getProperty "user.name")
                             :problem (:name @problem) :seed seed
                             :overview (slurp "overview.markdown")}
                            (git-meta-info)))
      (println "done.")
      (println
        (format "Running %d parameters, %d repetitions = %d simulations..."
                (count paired-params) repetitions (* (count paired-params) repetitions)))
      (run-partitions paired-params recdir nthreads monitor? repetitions)
      (println "Done."))
    (catch java.util.concurrent.ExecutionException e
      (println "Quitting early."))))
