(ns retrospect.core
  (:gen-class)
  (:use [clojure.contrib.command-line :only [with-command-line]])
  (:use [clojure.contrib.shell :only [sh]])
  (:use [clojure.contrib.string :only [split-lines]])
  (:use [retrospect.random])
  (:require [retrospect.state :as state])
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
     [seed "Seed" "0"]
     [database "Database identifier" "http://sisyphus:sisyphus@127.0.0.1:5984/retrospect"]]
    (let [seed (Integer/parseInt seed)]
      (set-seed seed)
      (dosync
       (alter state/datadir (constantly datadir))
       (alter state/database (constantly database)))
      (cond (and (not= action "player") (= "" params))
            (println "--params identifier required.")
            
            (= action "player")
            (let [prob (cond (or (= "Tracking" problem) (= "tracking" problem))
                             tracking-problem
                             (or (= "Words" problem) (= "words" problem))
                             words-problem)]
              (dosync
               (alter state/problem (constantly prob)))
              (start-player))
            
            (= action "run")
            (let [nthreads (Integer/parseInt nthreads)
                  repetitions (Integer/parseInt repetitions)
                  monitor? (Boolean/parseBoolean monitor)
                  ps (read-params params)
                  prob (cond (= "Tracking" (:problem ps)) tracking-problem
                             (= "Words" (:problem ps)) words-problem)
                  git-dirty? (not-empty (filter #(not= "??" (subs % 0 2))
                                                (split-lines (sh "git" "status" "--porcelain"))))]
              (when git-dirty?
                (println "Project has uncommitted changes. Commit with git before running simulations.")
                (System/exit -1))
              (dosync
               (alter state/problem (constantly prob))
               (alter state/params (constantly ps)))
              (run-with-new-record seed recordsdir nthreads monitor? repetitions))
            
            :else
            (println "No action given.")))))
