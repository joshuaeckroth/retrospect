(ns retrospect.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only [with-command-line]])
  (:use [retrospect.random])
  (:use [retrospect.database :only [read-params]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  ;(:use [retrospect.problems.circuit.problem :only [circuit-problem]])
  (:use [retrospect.records :only [run-with-new-record]])
  (:use [retrospect.player :only [start-player]]))

(defn -main [& args]
  (with-command-line args
    "retrospect"
    [[action "Action (run/player)" "player"]
     [problem "Problem" "tracking"]
     [params "Parameters identifier (e.g. 'Words/foobar')" ""]
     [datadir "Data directory" "data"]
     [recordsdir "Records directory" "records"]
     [record "Record" ""]
     [nthreads "Number of threads" "1"]
     [repetitions "Number of repetitions" "10"]
     [monitor "Activate monitor?" "false"]
     [seed "Seed" "0"]]
    (if (= "" params)
      (println "--params identifier required.")
      (let [nthreads (Integer/parseInt nthreads)
            repetitions (Integer/parseInt repetitions)
            monitor? (Boolean/parseBoolean monitor)
            seed (Integer/parseInt seed)
            ps (read-params params)
            problem (cond (= "Tracking" (:problem ps)) tracking-problem
                          (= "Words" (:problem ps)) words-problem)]
        (set-seed seed)
        (case action
              "run"
              (run-with-new-record problem ps seed datadir recordsdir
                nthreads monitor? repetitions)
              "player"
              (start-player problem datadir)

              (println "No action given."))))))


