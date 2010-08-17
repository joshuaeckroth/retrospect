(ns simulator.runners.hadoop
  (:require [clojure-hadoop.gen :as gen])
  (:require [clojure-hadoop.imports :as imp])
  (:import (org.apache.hadoop.util Tool))
  (:use [clojure.java.io :as io :only (writer)])
  (:use [simulator.types.problem :only (average-some-runs get-headers)])
  (:use [simulator.problems.tracking.core :only (tracking-problem)]))

(imp/import-io)
(imp/import-fs)
(imp/import-mapred)

(gen/gen-job-classes)

(def *input*)
(def *output*)
(def *problem*)

(defn write-input
  [params input]
  "This function only writes the input file."
  (with-open [writer (io/writer input)]
    (doseq [p params] (.write writer (str (print-str p) \newline)))))

(defn to-csv
  [row]
  (apply str (concat (interpose "," (map (fn [h] (h row))
					 (get-headers *problem*)))
		     [\newline])))

(defn mapper-map [this wkey wvalue #^OutputCollector output reporter]
  (doseq [result (average-some-runs *problem* [(read-string (.toString wvalue))] 10)]
    (.collect output (Text.) (Text. (to-csv result)))))

(defn reducer-reduce [this wkey wvalues #^OutputCollector output reporter]
  (.collect output wkey (reduce str (map (fn [#^Text v] (.toString v)) (iterator-seq wvalues)))))

(defn tool-run
  [#^Tool this args]
  (doto (JobConf. (.getConf this) (.getClass this))
    (.setJobName "simulator")
    (.setOutputKeyClass Text)
    (.setOutputValueClass Text)
    (.setMapperClass (Class/forName "simulator.runners.hadoop_mapper"))
    (.setReducerClass (Class/forName "simulator.runners.hadoop_reducer"))
    (.setInputFormat TextInputFormat)
    (.setOutputFormat TextOutputFormat)
    (FileOutputFormat/setCompressOutput false)
    (FileInputFormat/setInputPaths *input*)
    (FileOutputFormat/setOutputPath (Path. *output*))
    (JobClient/runJob))
  0)

(defn run-hadoop
  [problem params dir & args]
  (def *input* (str dir "/input.txt"))
  (def *output* (str dir "/hadoop-out"))
  (def *problem* problem)
  (write-input params *input*)
  (System/exit
   (org.apache.hadoop.util.ToolRunner/run
    (new org.apache.hadoop.conf.Configuration)
    (. (Class/forName "simulator.runners.hadoop") newInstance)
    (into-array String args))))
