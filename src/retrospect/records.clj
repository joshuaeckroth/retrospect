(ns retrospect.records
  (:import [java.io File])
  (:use [granary.misc :only [format-date-ms]])
  (:use [granary.git :only [git-meta-info]])
  (:use [granary.runs :only [commit-run]])
  (:use [granary.parameters :only [explode-params vectorize-params]])
  (:require [clojure.string :as str])
  (:use [clojure.java.io :only [file]])
  (:use [clojure.java.shell :only [sh]])
  (:use [retrospect.local :only [run-partitions]])
  (:use [clojure-csv.core :only [parse-csv]])
  (:use [retrospect.state]))

(defn read-csv
  [lines]
  (let [headers (map keyword (str/split (first lines) #","))]
    (doall
     (for [line (parse-csv (str/join "\n" (rest lines)))]
       (let [data (map #(cond (re-matches #"^(true|false)$" %) (Boolean/parseBoolean %)
                            (re-matches #"^-?\d+\.\d+E?-?\d*$" %) (Double/parseDouble %)
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
                                     (file-seq (file recdir))))))]
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
    (commit-run run-meta results)
    (println "Done.")))

(defn run-with-new-record
  "Create a new folder for storing run data and execute the run."
  [seed git recordsdir nthreads upload? save-record? repetitions]
  (try
    (let [t (. System (currentTimeMillis))
          recdir (str recordsdir "/" t)
          control-params (explode-params (vectorize-params (:control @db-params)))
          comparison-params (when (:comparison @db-params)
                              (explode-params (vectorize-params (:comparison @db-params))))
          paired-params (when comparison-params
                          (partition 2 (interleave control-params comparison-params)))
          run (merge {:starttime (format-date-ms t)
                      :paramid (:paramid @db-params)
                      :datadir @datadir :recorddir recdir :nthreads nthreads
                      :pwd (:out (sh "pwd")) :repetitions repetitions :seed seed
                      :hostname (.getHostName (java.net.InetAddress/getLocalHost))
                      :username (System/getProperty "user.name")}
                     (git-meta-info git (:out (sh "pwd"))))]
      (when (and comparison-params (not= (count control-params) (count comparison-params)))
        (println "Control/comparison param counts are not equal.")
        (System/exit -1))
      (when save-record?
        (print (format "Making new directory %s..." recdir))
        (.mkdirs (File. recdir))
        (println "done."))
      #_(println (format "Running %d parameters, %d repetitions = %d simulations..."
                       (count control-params) repetitions
                       (* (count control-params) repetitions)))
      (doall (run-partitions run (not (nil? comparison-params))
                             (if comparison-params paired-params control-params)
                             recdir nthreads save-record? repetitions))
      (when (and upload? (not= "" "localhost"))
        (submit-archived-results recdir))
      (System/exit 0))))
