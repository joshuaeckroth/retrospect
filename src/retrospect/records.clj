(ns retrospect.records
  (:import [java.io File])
  (:use [clojure.java.io :as io :only (writer copy file)])
  (:import [java.util Date])
  (:require [clojure.string :as str])
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
  (:use [clojure-csv.core :only [parse-csv]])
  (:use [retrospect.state]))

(defn vectorize-params
  [params]
  (reduce (fn [m k] (let [v (k params)]
                      (assoc m k (if (vector? v) v [v]))))
          {} (keys params)))

(defn explode-params
  "Want {:Xyz [1 2 3], :Abc [3 4]} to become [{:Xyz 1, :Abc 3}, {:Xyz 2, :Abc 4}, ...]"
  [params]
  (when (not-empty params)
    (if (= 1 (count params))
      (for [v (second (first params))]
        {(first (first params)) v})
      (let [p (first params)
            deeper (explode-params (rest params))]
        (flatten (map (fn [v] (map #(assoc % (first p) v) deeper)) (second p)))))))

(defn git-meta-info
  [git]
  (let [[commit _ _ _ & msg] (split-lines (sh git "log" "-n" "1"))
        branch (trim (subs (sh git "branch" "--contains") 2))]
    {:commit (subs commit 7)
     :commit-msg (apply str (interpose "\n" (map (fn [s] (subs s 4)) (filter not-empty msg))))
     :branch branch}))

(defn read-csv
  [lines]
  (let [headers (map keyword (str/split (first lines) #","))]
    (doall
     (for [line (parse-csv (str/join "\n" (rest lines)))]
       (let [data (map #(cond (re-matches #"^(true|false)$" %) (Boolean/parseBoolean %)
                            (re-matches #"^\d+\.\d+$" %) (Double/parseDouble %)
                            (re-matches #"^\d+$" %) (Integer/parseInt %)
                            :else %)
                     line)]
         (apply hash-map (interleave headers data)))))))

(defn read-archived-results
  [recdir]
  (let [simulations (sort (set (map #(Integer/parseInt
                                    (second (re-matches #".*\-(\d+)\.csv$"
                                                        (.getName %))))
                                  (filter #(re-find #"\.csv$" (.getName %))
                                     (file-seq (clojure.java.io/file recdir))))))]
    (doall
     (for [sim simulations]
       (let [control-file
             (format "%s/control-results-%d.csv" recdir sim)
             comparison-file
             (format "%s/comparison-results-%d.csv" recdir sim)
             comparative-file
             (format "%s/comparative-results-%d.csv" recdir sim)]
         {:control [(last (read-csv (str/split (slurp control-file) #"\n")))]
          :comparison (when (. (file comparison-file) exists)
                        [(last (read-csv (str/split (slurp comparison-file) #"\n")))])
          :comparative (when (. (file comparative-file) exists)
                         [(last (read-csv (str/split (slurp comparative-file) #"\n")))])})))))

(defn submit-archived-results
  [recdir]
  (let [run-meta (read-string (slurp (format "%s/meta.clj" recdir)))
        results (read-archived-results recdir)]
    (println "Writing results to database...")
    (db/commit-run run-meta results)
    (println "Done.")))

(defn run-with-new-record
  "Create a new folder for storing run data and execute the run."
  [seed git recordsdir nthreads upload? save-record? repetitions]
  (try
    (let [t (. System (currentTimeMillis))
          recdir (str recordsdir "/" t)
          comparative? (= "comparative" (:paramstype @db-params))
          control-params (explode-params (vectorize-params (:control @db-params)))
          comparison-params (when comparative?
                              (explode-params (vectorize-params (:comparison @db-params))))
          paired-params (when comparative?
                          (partition 2 (interleave control-params comparison-params)))
          run (merge {:type "run" :time t :paramsid (:_id @db-params)
                      :paramsrev (:_rev @db-params)
                      :paramsname (format "%s/%s" (:name @problem) (:name @db-params))
                      :paramstype (:paramstype @db-params)
                      :database @database
                      :datadir @datadir :recorddir recdir :nthreads nthreads
                      :pwd (pwd) :repetitions repetitions
                      :hostname (.getHostName (java.net.InetAddress/getLocalHost))
                      :username (System/getProperty "user.name")
                      :problem (:name @problem) :seed seed
                      :overview (slurp "overview.markdown")}
                     (git-meta-info git))]
      (when (and comparative? (not= (count control-params) (count comparison-params)))
        (println "Control/comparison param counts are not equal.")
        (System/exit -1))
      (when save-record?
        (print (format "Making new directory %s..." recdir))
        (.mkdirs (File. recdir))
        (println "done."))
      #_(println (format "Running %d parameters, %d repetitions = %d simulations..."
                       (count control-params) repetitions
                       (* (count control-params) repetitions)))
      (doall (run-partitions run comparative? (if comparative? paired-params control-params)
                             recdir nthreads save-record? repetitions))
      (when (and upload? (not= "" @database))
        (submit-archived-results recdir))
      (System/exit 0))
    (catch java.util.concurrent.ExecutionException e
      (println "Quitting early."))))
