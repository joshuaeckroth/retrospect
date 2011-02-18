(ns samre.runners.hadoop
  (:require [clojure-hadoop.gen :as gen])
  (:require [clojure-hadoop.imports :as imp])
  (:import (org.apache.hadoop.util Tool))
  (:use [samre.problem :only (average-runs get-headers)])
  (:use [samre.problems.tracking.problem :only (tracking-problem)]))

(imp/import-io)
(imp/import-fs)
(imp/import-mapred)

(gen/gen-job-classes)

(def *input*)
(def *output*)
(def *problem*)

(defn to-csv
  [row]
  (apply str (concat (interpose "," (map (fn [h] (h row))
					 (get-headers tracking-problem)))
		     [\newline])))

;; MAY NO LONGER WORK
(defn mapper-map [this wkey wvalue #^OutputCollector output reporter]
  (doseq [result (average-runs tracking-problem
                               [(read-string (.toString wvalue))] 10)]
    (.collect output (Text.) (Text. (to-csv result)))))

(defn reducer-reduce [this wkey wvalues #^OutputCollector output reporter]
  (.collect output wkey (reduce str (map (fn [#^Text v] (.toString v))
                                         (iterator-seq wvalues)))))

(defn tool-run
  [#^Tool this args]
  (doto (JobConf. (.getConf this) (.getClass this))
    (.setJobName "samre")
    (.setOutputKeyClass Text)
    (.setOutputValueClass Text)
    (.setMapperClass (Class/forName "samre.runners.hadoop_mapper"))
    (.setReducerClass (Class/forName "samre.runners.hadoop_reducer"))
    (.setInputFormat TextInputFormat)
    (.setOutputFormat TextOutputFormat)
    (FileOutputFormat/setCompressOutput false)
    (FileInputFormat/setInputPaths *input*)
    (FileOutputFormat/setOutputPath (Path. *output*))
    (JobClient/runJob))
  0)

(defn run-hadoop [problem input output args]
  (def *input* input)
  (def *output* output)
  (def *problem* problem)
  (System/exit
   (org.apache.hadoop.util.ToolRunner/run
    (new org.apache.hadoop.conf.Configuration)
    (. (Class/forName "samre.runners.hadoop") newInstance)
    (into-array String args))))
