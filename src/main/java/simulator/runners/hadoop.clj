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
(gen/gen-main-method)

(defn run-hadoop2
  [problem params dir]
  "This function only writes the input file."
  (with-open [writer (io/writer (str dir "/input.txt"))]
    (doseq [p params] (.write writer (str (print-str p) \newline)))))

(defn to-csv
  [row]
  (apply str (concat (interpose "," (map (fn [h] (h row))
					 (get-headers tracking-problem)))
		     [\newline])))

(defn mapper-map [this wkey wvalue #^OutputCollector output reporter]
  (doseq [result (average-some-runs tracking-problem [(read-string (.toString wvalue))] 10)]
    (.collect output (LongWritable. 0) (Text. (to-csv result)))))

(defn reducer-reduce [this wkey wvalues #^OutputCollector output reporter]
  (.collect output wkey (reduce str (map (fn [#^Text v] (.toString v)) (iterator-seq wvalues)))))

(defn tool-run
  [#^Tool this args]
  (doto (JobConf. (.getConf this) (.getClass this))
    (.setJobName "simulator")
    (.setOutputKeyClass LongWritable)
    (.setOutputValueClass Text)
    (.setMapperClass (Class/forName "simulator.runners.hadoop_mapper"))
    (.setReducerClass (Class/forName "simulator.runners.hadoop_reducer"))
    (.setInputFormat TextInputFormat)
    (.setOutputFormat TextOutputFormat)
    (FileOutputFormat/setCompressOutput false)
    (FileInputFormat/setInputPaths (first args))
    (FileOutputFormat/setOutputPath (Path. (second args)))
    (JobClient/runJob))
  0)
