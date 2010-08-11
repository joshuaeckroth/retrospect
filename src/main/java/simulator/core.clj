(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.problems.tracking.core :only (tracking-problem)])
  (:use [simulator.records :only (run-with-new-record list-records)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/list/player)"]
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
	    "list"
	    (list-records recordsdir)
	    "player"
	    (if (= problem "tracking") nil ;;(start-player tracking-problem)
		(println "Player only available for 'tracking' problem."))
	    (println "No action given.")))))


