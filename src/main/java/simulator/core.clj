(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.problems.tracking.core :only (tracking-problem)])
  (:use [simulator.problems.tracking.player :as tracking-player :only (start-player)])
  (:use [simulator.records
	 :only (run-with-new-record list-records prepare-hadoop cleanup-hadoop-results)])
  (:use [simulator.runners.hadoop :only (run-hadoop)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/list/player/prepare-hadoop/hadoop/clean-hadoop)"]
     [problem "Problem" "tracking"]
     [paramsfile "Parameters XML file" "params.xml"]
     [recordsdir "Records directory" "c:/users/josh/documents/research/simulator-records"]
     [nthreads "Number of threads" "4"]
     [input "Hadoop input" ""]
     [output "Hadoop output" ""]]
    (let [nthreads (Integer/parseInt nthreads)
	  prob tracking-problem]
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
	    "player"
	    (if (= problem "tracking") (tracking-player/start-player)
		(println "Player only available for 'tracking' problem."))
	    (println "No action given.")))))


