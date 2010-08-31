(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.problems.tracking.problem :only (tracking-problem)])
  (:use [simulator.records
	 :only (run-with-new-record list-records prepare-hadoop cleanup-hadoop-results)])
  (:use [simulator.runners.hadoop :only (run-hadoop)])
  (:use [simulator.types.problem :only (start-player)])
  (:use [clj-stacktrace.repl :only (pst+)]))

(defn -main [& args]
  (try
    (with-command-line args
      "Simulator"
      [[action "Action (run/list/player/prepare-hadoop/hadoop/clean-hadoop)"]
       [problem "Problem" "tracking"]
       [paramsfile "Parameters XML file" "params.xml"]
       [recordsdir "Records directory" "records"]
       [nthreads "Number of threads" "8"]
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
	      (start-player prob)

	      (println "No action given."))))
    (catch Exception e (pst+ e))))


