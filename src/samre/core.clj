(ns samre.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [samre.problems.tracking.problem :only (tracking-problem)])
  ;(:use [samre.problems.circuit.problem :only (circuit-problem)])
  (:use [samre.records
	 :only (run-with-new-record list-records chart
                 prepare-hadoop cleanup-hadoop-results)])
  (:use [samre.runners.hadoop :only (run-hadoop)])
  (:use [samre.player.gui :only (start-player)])
  (:require [swank.swank]))

(defn -main [& args]
  (with-command-line args
    "SAMRE"
    [[action "Action (run/list/player/chart/prepare-hadoop/hadoop/clean-hadoop)" "player"]
     [problem "Problem" "tracking"]
     [paramsfile "Parameters XML file" "params.xml"]
     [recordsdir "Records directory" "records"]
     [record "Record" ""]
     [nthreads "Number of threads" "1"]
     [monitor "Activate monitor?" "false"]
     [input "Hadoop input" ""]
     [output "Hadoop output" ""]]
    (let [nthreads (Integer/parseInt nthreads)
          monitor (Boolean/parseBoolean monitor)
          prob (cond (= problem "tracking") tracking-problem
                     ;(= problem "circuit") circuit-problem
                     :else nil)]
      (case action
            "run"
            (run-with-new-record prob paramsfile recordsdir nthreads monitor)
            "prepare-hadoop"
            (prepare-hadoop prob paramsfile recordsdir)
            "hadoop"
            (run-hadoop prob input output args)
            "clean-hadoop"
            (cleanup-hadoop-results prob recordsdir
                                    (str recordsdir "/results-hadoop.csv")
                                    (str recordsdir "/results.csv"))
            "list"
            (list-records recordsdir)
            "chart"
            (chart recordsdir record prob)
            "charts"
            (chart recordsdir record prob)
            "player"
            (do
              (swank.swank/start-repl 4006)
              (start-player prob))

            (println "No action given.")))))


