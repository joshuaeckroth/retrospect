(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.runner :only (multiple-runs save-results)])
  (:use [simulator.tracking :as tracking :only (generate-run-params run)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[action "Action (run/plot/playback)"]
     [problem "Problem" "tracking"]
     [recordsdir "Records directory" "."]]
    (case action
	  "run"
	  (if (= problem "tracking")
	    (save-results (multiple-runs (tracking/generate-run-params) tracking/run)))
	  "plot"
	  (println "Plot...")
	  "playback"
	  (println "Playback...")
	  (println "No action given."))))


