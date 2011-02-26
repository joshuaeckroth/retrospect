(ns samre.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [samre.problems.tracking.problem :only (tracking-problem)])
  ;(:use [samre.problems.circuit.problem :only (circuit-problem)])
  (:use [samre.records :only [run-with-new-record list-records]])
  (:use [samre.player.gui :only (start-player)])
  (:require [swank.swank]))

(defn -main [& args]
  (with-command-line args
    "SAMRE"
    [[action "Action (run/list/player)" "player"]
     [problem "Problem" "tracking"]
     [paramsfile "Parameters XML file" "params.xml"]
     [recordsdir "Records directory" "records"]
     [record "Record" ""]
     [nthreads "Number of threads" "1"]
     [repetitions "Number of repetitions" "10"]
     [monitor "Activate monitor?" "false"]]
    (let [nthreads (Integer/parseInt nthreads)
          repetitions (Integer/parseInt repetitions)
          monitor (Boolean/parseBoolean monitor)
          prob (cond (= problem "tracking") tracking-problem
                     ;(= problem "circuit") circuit-problem
                     :else nil)]
      (case action
            "run"
            (run-with-new-record prob paramsfile recordsdir nthreads monitor repetitions)
            "list"
            (list-records recordsdir)
            "player"
            (do
              (swank.swank/start-repl 4006)
              (start-player prob))

            (println "No action given.")))))


