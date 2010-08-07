(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.runner :only (multiple-runs save-results)])
  (:use [simulator.tracking :as tracking :only (generate-params run)])
  (:use [simulator.records :only (run-with-new-record list-records)])
  (:use [simulator.tracking.player :only (start-player)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/list/player)"]
     [problem "Problem" "tracking"]
     [recordsdir "Records directory" "c:/users/josh/documents/research/simulator-records"]
     [nthreads "Number of threads" 4]]
    (case action
	  "run"
	  (if (= problem "tracking")
	    (run-with-new-record recordsdir (tracking/generate-params) tracking/run nthreads))
	  "list"
	  (list-records recordsdir)
	  "player"
	  (if (= problem "tracking") (start-player)
        (println "Player only available for 'tracking' problem."))
	  (println "No action given."))))


