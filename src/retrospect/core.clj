(ns retrospect.core
  (:import (javax.swing SwingUtilities))
  (:use [clojure.java.shell :only [sh]])
  (:use [clojure.string :only [split-lines]])
  (:use [clojure.tools.cli :only [cli]])
  (:use [granary.random :only [rgen new-seed]])
  (:require [retrospect.state :as state])
  (:use [granary.misc])
  (:use [granary.parameters :only [read-params]])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  (:use [retrospect.problems.classify.problem :only [classify-problem]])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.simulate :only [run]])
  (:use [granary.records :only [run-with-new-record submit-archived-results]])
  (:use [retrospect.explore :only [start-explore]])
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.utility]))

;; Retrospect is a folder name but used as namespace to refer to files in that folder

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
  (let [[options _ banner]
        (cli args
             ["--action" "Action (run/player/explore/resubmit)" :default "player"]
             ["--reasoner" "Reasoning algorithm" :default "abduction"]
             ["--problem" "Problem" :default "tracking"]
             ["--params" "Parameters identifier (e.g. 'Words/foobar')" :default ""]
             ["--datadir" "Data directory" :default "data"]
             ["--recordsdir" "Records directory" :default "records"]
             ["--nthreads" "Number of threads" :default 1 :parse-fn #(Integer. %)]
             ["--repetitions" "Number of repetitions" :default 10 :parse-fn #(Integer. %)]
             ["--git" "Git path" :default "git"]
             ["--seed" "Seed" :default 0 :parse-fn #(Integer. %)]
             ["--dbhost" "MySQL database host" :default "localhost"]
             ["--dbname" "MySQL database name" :default "retrospect"]
             ["--dbuser" "MySQL database user" :default "user"]
             ["--dbpassword" "MySQL database password" :default "password"]
             ["--upload" "Upload?" :default true :parse-fn #(= "true" %)]
             ["--save-record" "Save in record directory?" :default true :parse-fn #(= "true" %)]
             ["--recdir" "Record directory" :default ""]
             ["--log" "Show verbose logging?" :default false :parse-fn #(= "true" %)])
        reasoner (choose-reasoner (:reasoner options))
        problem (choose-problem (:problem options))]
    (set-granary-db (:dbhost options) (:dbname options) (:dbuser options) (:dbpassword options))
    (alter-var-root (var rgen) (constantly (new-seed (:seed options))))
    (dosync
     (alter state/datadir (constantly (:datadir options)))
     (alter state/reasoner (constantly reasoner))
     (alter state/problem (constantly problem))
     (alter state/logging-enabled (constantly (:log options))))
    (cond (and (= (:action options) "run") (= "" (:params options)))
          (println "--params identifier required.")
          
          (= (:action options) "player")
          ;; start the player on swing's "event dispatch thread"
          (SwingUtilities/invokeLater start-player)

          (= (:action options) "explore")
          (do
            (dosync (alter state/batch (constantly true)))
            ;; start the explore gui on swing's "event dispatch thread"
            (SwingUtilities/invokeLater start-explore))

          (= (:action options) "resubmit")
          (submit-archived-results (:recdir options))
          
          (= (:action options) "run")
          (let [[problem-name ps] (read-params (:params options))
                problem (choose-problem problem-name)
                git-dirty? (not-empty
                            (filter #(not= "??" (if (>= 2 (count %)) "??" (subs % 0 2)))
                               (split-lines (:out (sh (:git options) "status" "--porcelain")))))]
            (when (or (nil? problem) (nil? ps))
              (println "No such parameters.")
              (System/exit -1))
            (when (and (:upload options) git-dirty?)
              (println "Project has uncommitted changes. Commit with git before"
                       "running simulations.")
              (System/exit -1))
            (dosync
             (alter state/batch (constantly true))
             (alter state/problem (constantly problem)))
            (run-with-new-record run ps (:datadir options) (:seed options)
              (:git options) (:recordsdir options) (:nthreads options)
              (:upload options) (:save-record options) (:repetitions options)))
          
          :else
          (println "No action given."))))
