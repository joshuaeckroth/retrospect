(ns retrospect.core
  (:gen-class)
  (:import (javax.swing SwingUtilities))
  (:use [clojure.contrib.command-line :only [with-command-line]])
  (:use [clojure.contrib.shell :only [sh]])
  (:use [clojure.contrib.string :only [split-lines]])
  (:use [retrospect.random :only [rgen new-seed]])
  (:require [retrospect.state :as state])
  (:use [retrospect.database :only [read-params]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  (:use [retrospect.problems.causal.problem :only [causal-problem]])
  (:use [retrospect.records :only [run-with-new-record]])
  (:use [retrospect.explore :only [explore]])
  (:use [retrospect.player :only [start-player]]))

(defn choose-problem
  [problem]
  (cond (or (= "Tracking" problem) (= "tracking" problem))
        tracking-problem
        (or (= "Words" problem) (= "words" problem))
        words-problem
        (or (= "Causal" problem) (= "causal" problem))
        causal-problem))

(defn -main [& args]
  (with-command-line args
    "retrospect"
    [[action "Action (run/player/explore)" "player"]
     [problem "Problem" "tracking"]
     [params "Parameters identifier (e.g. 'Words/foobar')" ""]
     [datadir "Data directory" "data"]
     [recordsdir "Records directory" "records"]
     [nthreads "Number of threads" "1"]
     [repetitions "Number of repetitions" "10"]
     [monitor "Activate monitor?" "false"]
     [git "Git path" "git"]
     [seed "Seed" "0"]
     [database "Database identifier" "http://127.0.0.1:5984/retrospect"]
     [upload "Upload?" "true"]
     [metric "Explore metric to optimize" "Milliseconds"]
     [min-max "Optimization goal (min/max)" "min"]
     [restarts "# of explore restarts" "5"]
     [permutations "# of explore permutations" "100"]]
    (let [seed (Integer/parseInt seed)
          prob (choose-problem problem)
          repetitions (Integer/parseInt repetitions)]
      (alter-var-root (var rgen) (constantly (new-seed seed)))
      (dosync
       (alter state/datadir (constantly datadir))
       (alter state/database (constantly database))
       (alter state/problem (constantly prob)))
      (cond (and (not= action "explore") (not= action "player") (= "" params))
            (println "--params identifier required.")
            
            (= action "player")
            ;; start the player on swing's "event dispatch thread"
            (SwingUtilities/invokeLater start-player)

            (= action "explore")
            (let [restarts (Integer/parseInt restarts)
                  permutations (Integer/parseInt permutations)]
              (explore seed (keyword metric) min-max repetitions restarts permutations))
            
            (= action "run")
            (let [nthreads (Integer/parseInt nthreads)
                  monitor? (Boolean/parseBoolean monitor)
                  upload? (Boolean/parseBoolean upload)
                  [prob ps] (read-params params)
                  git-dirty? (not-empty
                              (filter #(not= "??" (subs % 0 2))
                                      (split-lines (sh git "status" "--porcelain"))))]
              (when (and upload? git-dirty?
                         (not (or (re-matches #".*127\.0\.0\.1.*" database)
                                  (re-matches #".*localhost.*" database))))
                (println "Project has uncommitted changes. Commit with git before"
                         "running simulations, or use the default (localhost)"
                         "database connection.")
                (System/exit -1))
              (dosync
               (alter state/problem (constantly (choose-problem prob)))
               (alter state/db-params (constantly ps)))
              (run-with-new-record seed git recordsdir nthreads monitor? upload? repetitions))
            
            :else
            (println "No action given.")))))
