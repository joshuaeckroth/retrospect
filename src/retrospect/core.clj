(ns retrospect.core
  (:gen-class)
  (:import (javax.swing SwingUtilities))
  (:use [clojure.contrib.command-line :only [with-command-line]])
  (:use [clojure.contrib.shell :only [sh]])
  (:use [clojure.contrib.string :only [split-lines]])
  (:use [retrospect.random :only [rgen new-seed]])
  (:require [retrospect.state :as state])
  (:use [granary.misc])
  (:use [granary.parameters :only [read-params]])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  (:use [retrospect.problems.classify.problem :only [classify-problem]])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.records :only [run-with-new-record submit-archived-results]])
  (:use [retrospect.explore :only [start-explore]])
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.bugreport]))

(defn choose-problem
  [problem]
  (cond (or (= "Tracking" problem) (= "tracking" problem))
        tracking-problem
        (or (= "Words" problem) (= "words" problem))
        words-problem
        (or (= "Classify" problem) (= "classify" problem))
        classify-problem
        (or (= "AbdExp" problem) (= "abdexp" problem))
        abdexp-problem))

(defn choose-reasoner
  [reasoner]
  (cond (or (= "Abduction" reasoner) (= "abduction" reasoner))
        reason-abduction))

(defn -main [& args]
  (set-exception-handler)
  (with-command-line args
    "retrospect"
    [[action "Action (run/player/explore/resubmit)" "player"]
     [reasoner "Reasoning algorithm" "abduction"]
     [problem "Problem" "tracking"]
     [params "Parameters identifier (e.g. 'Words/foobar')" ""]
     [datadir "Data directory" "data"]
     [recordsdir "Records directory" "records"]
     [nthreads "Number of threads" "1"]
     [repetitions "Number of repetitions" "10"]
     [git "Git path" "git"]
     [seed "Seed" "0"]
     [dbhost "MySQL database host" "localhost"]
     [dbname "MySQL database name" "retrospect"]
     [dbuser "MySQL database user" "user"]
     [dbpassword "MySQL database password" "password"]
     [upload "Upload?" "true"]
     [save-record "Save in record directory?" "true"]
     [recdir "Record directory" ""]
     [log "Show verbose logging?" "false"]]
    (let [seed (Integer/parseInt seed)
          reasoner (choose-reasoner reasoner)
          problem (choose-problem problem)
          repetitions (Integer/parseInt repetitions)
          logging-enabled (Boolean/parseBoolean log)]
      (set-granary-db dbhost dbname dbuser dbpassword)
      (alter-var-root (var rgen) (constantly (new-seed seed)))
      (dosync
       (alter state/datadir (constantly datadir))
       (alter state/reasoner (constantly reasoner))
       (alter state/problem (constantly problem))
       (alter state/logging-enabled (constantly logging-enabled)))
      (cond (and (= action "run") (= "" params))
            (println "--params identifier required.")
            
            (= action "player")
            ;; start the player on swing's "event dispatch thread"
            (SwingUtilities/invokeLater start-player)

            (= action "explore")
            (do
              (dosync (alter state/batch (constantly true)))
              ;; start the explore gui on swing's "event dispatch thread"
              (SwingUtilities/invokeLater start-explore))

            (= action "resubmit")
            (submit-archived-results recdir)
            
            (= action "run")
            (let [nthreads (Integer/parseInt nthreads)
                  upload? (Boolean/parseBoolean upload)
                  save-record? (Boolean/parseBoolean save-record)
                  [problem ps] (read-params params)
                  git-dirty? (not-empty
                              (filter #(not= "??" (if (>= 2 (count %)) "??" (subs % 0 2)))
                                 (split-lines (sh git "status" "--porcelain"))))]
              (when (or (nil? problem) (nil? ps))
                (println "No such parameters.")
                (System/exit -1))
              (when (and upload? git-dirty?)
                (println "Project has uncommitted changes. Commit with git before"
                         "running simulations.")
                (System/exit -1))
              (dosync
               (alter state/batch (constantly true))
               (alter state/problem (constantly (choose-problem problem)))               
               (alter state/db-params (constantly ps)))
              (run-with-new-record seed git recordsdir nthreads
                upload? save-record? repetitions))
            
            :else
            (println "No action given.")))))
