(ns retrospect.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only [with-command-line]])
  (:use [retrospect.random])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  ;(:use [retrospect.problems.circuit.problem :only [circuit-problem]])
  (:use [retrospect.records :only [run-with-new-record list-records]])
  (:use [retrospect.player :only [start-player]]))

(defn -main [& args]
  (with-command-line args
    "retrospect"
    [[action "Action (run/list/player)" "player"]
     [problem "Problem" "tracking"]
     [paramsfile "Parameters XML file" "params.xml"]
     [datadir "Data directory" "data"]
     [recordsdir "Records directory" "records"]
     [record "Record" ""]
     [nthreads "Number of threads" "1"]
     [repetitions "Number of repetitions" "10"]
     [monitor "Activate monitor?" "false"]
     [meta "Meta-abduction?" "true"]
     [seed "Seed" "0"]]
    (let [nthreads (Integer/parseInt nthreads)
          repetitions (Integer/parseInt repetitions)
          monitor? (Boolean/parseBoolean monitor)
          meta? (Boolean/parseBoolean meta)
          seed (Integer/parseInt seed)
          prob (cond (= problem "tracking") tracking-problem
                     (= problem "words") words-problem
                     ;(= problem "circuit") circuit-problem
                     :else nil)]
      (set-seed seed)
      (case action
            "run"
            (run-with-new-record prob paramsfile datadir recordsdir nthreads
              monitor? meta? repetitions)
            "list"
            (list-records recordsdir)
            "player"
            (start-player prob datadir)

            (println "No action given.")))))


