(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.problems.tracking.problem :only (tracking-problem)])
  ;(:use [simulator.problems.circuit.problem :only (circuit-problem)])
  (:use [simulator.records
	 :only (run-with-new-record list-records chart
                 prepare-hadoop cleanup-hadoop-results)])
  (:use [simulator.runners.hadoop :only (run-hadoop)])
  (:use [simulator.problem :only (start-player)])
  (:require [swank.swank]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/list/player/chart/prepare-hadoop/hadoop/clean-hadoop)"]
     [problem "Problem" "tracking"]
     [paramsfile "Parameters XML file" "params.xml"]
     [recordsdir "Records directory" "records"]
     [record "Record" ""]
     [nthreads "Number of threads" "8"]
     [input "Hadoop input" ""]
     [output "Hadoop output" ""]]
    (let [nthreads (Integer/parseInt nthreads)
          prob (cond (= problem "tracking") tracking-problem
                     ;(= problem "circuit") circuit-problem
                     :else nil)]
      (case action
            "run"
            (run-with-new-record prob paramsfile recordsdir nthreads)
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
            "player"
            (do
              (swank.swank/start-repl 4006)
              (start-player prob))

            (println "No action given.")))))


