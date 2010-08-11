(ns simulator.runners.hadoop
  (:require [clojure-hadoop.wrap :as wrap]
	    [clojure-hadoop.defjob :as defjob]
	    [clojure-hadoop.imports :as imp]))

(imp/import-io)
(imp/import-mapred)

(defn run-hadoop
  [problem params recordsdir nthreads])