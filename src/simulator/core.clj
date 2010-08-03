(ns simulator.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:use [simulator.runner :only (multiple-runs save-results)])
  (:use [simulator.tracking :as tracking :only (generate-run-params run)]))

(defn -main [& args]
  (with-command-line args
    "Simulator"
    [[problem "Problem" "tracking"]]
    (if (= problem "tracking")
      (save-results (multiple-runs (tracking/generate-run-params) tracking/run)))))


