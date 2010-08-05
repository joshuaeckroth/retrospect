(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.runner :only (multiple-runs save-results)])
  (:use [simulator.tracking :as tracking :only (generate-params run)])
  (:use [simulator.records :only (run-with-new-record)])
  (:use [simulator.tracking.player :only (start-player)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/plot/player)"]
     [problem "Problem" "tracking"]
     [recordsdir "Records directory" "c:/users/josh/documents/research/simulator/records"]]
    (case action
	  "run"
	  (if (= problem "tracking")
	    (run-with-new-record recordsdir (tracking/generate-params) tracking/run))
	  "plot"
	  (println "Plot...")
	  "player"
	  (if (= problem "tracking") (start-player)
        (println "Player only available for 'tracking' problem."))
	  (println "No action given."))))


