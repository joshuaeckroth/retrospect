(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.problems.tracking.core :only (tracking-problem)])
  (:use [simulator.problems.tracking.player :as tracking-player :only (start-player)])
  (:use [simulator.records :only (run-with-new-record list-records cleanup-hadoop-results)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/list/player/clean-hadoop)"]
     [hadoop "Use Hadoop? (0/1)" "0"]
     [problem "Problem" "tracking"]
     [paramsfile "Parameters XML file" "params.xml"]
     [recordsdir "Records directory" "c:/users/josh/documents/research/simulator-records"]
     [nthreads "Number of threads" "4"]]
    (let [hadoop (= 1 (Integer/parseInt hadoop))
	  nthreads (Integer/parseInt nthreads)]
      (case action
	    "run"
	    (if (= problem "tracking")
	      (run-with-new-record hadoop tracking-problem paramsfile recordsdir nthreads))
	    "clean-hadoop"
	    (if (= problem "tracking")
	      (cleanup-hadoop-results tracking-problem recordsdir
				      (str recordsdir "/results-hadoop.csv")
				      (str recordsdir "/results.csv")))
	    "list"
	    (list-records recordsdir)
	    "player"
	    (if (= problem "tracking") (tracking-player/start-player)
		(println "Player only available for 'tracking' problem."))
	    (println "No action given.")))))


