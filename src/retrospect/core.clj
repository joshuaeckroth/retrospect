(ns retrospect.core
  (:import (javax.swing SwingUtilities))
  (:use [clojure.java.shell :only [sh]])
  (:use [clojure.string :only [split-lines]])
  (:use [clojure.tools.cli :only [cli]])
  (:use [geppetto.random :only [rgen new-seed]])
  (:require [retrospect.state :as state])
  (:use propertea.core)
  (:use [geppetto.misc])
  (:use [geppetto.parameters :only [read-params extract-problem]])
  (:use [geppetto.claim])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  (:use [retrospect.problems.classify.problem :only [classify-problem]])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.simulate :only [run]])
  (:use [geppetto.records :only [run-with-new-record submit-archived-results]])
  (:use [retrospect.explore :only [start-explore]])
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.utility]))

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
             ["--action" "Action (run/player/explore/resubmit/verify-claims)" :default "player"]
             ["--reasoner" "Reasoning algorithm" :default "abduction"]
             ["--problem" "Problem" :default "tracking"]
             ["--params" "Parameters identifier (e.g. 'Words/foobar')" :default ""]
             ["--nthreads" "Number of threads" :default 1 :parse-fn #(Integer. %)]
             ["--repetitions" "Number of repetitions" :default 10 :parse-fn #(Integer. %)]
             ["--seed" "Seed" :default 0 :parse-fn #(Integer. %)]
             ["--upload" "Upload?" :default true :parse-fn #(= "true" %)]
             ["--save-record" "Save in record directory?" :default true :parse-fn #(= "true" %)]
             ["--recdir" "Record directory (to resubmit)" :default ""]
             ["--log" "Show verbose logging?" :default false :parse-fn #(= "true" %)]
             ["--quiet" "Quiet mode (hide progress messages)?" :default false :parse-fn #(= "true" %)])
        reasoner (choose-reasoner (:reasoner options))
        problem (choose-problem (:problem options))
        props (read-properties "config.properties")]
    (setup-geppetto (:geppetto_dbhost props)
                    (:geppetto_dbname props)
                    (:geppetto_dbuser props)
                    (:geppetto_dbpassword props)
                    (:quiet options))
    (alter-var-root (var rgen) (constantly (new-seed (:seed options))))
    (dosync
     (alter state/datadir (constantly (:datadir props)))
     (alter state/reasoner (constantly reasoner))
     (alter state/problem (constantly problem))
     (alter state/logging-enabled (constantly (:log options)))
     (alter state/quiet-mode (constantly (:quiet options))))
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

          (= (:action options) "verify-claims")
          (do
            (dosync (alter state/batch (constantly true)))
            (let [claims (concat (get @state/problem :claims [])
                                 (get-in @state/problem [(:key reasoner) :claims] []))]
              (if (empty? claims)
                (println (format "No claims for %s" (:name @state/problem)))
                (do
                  (println (format "Verifying %d claims for %s with reasoner %s..."
                              (count claims) (:name @state/problem) (:name @state/reasoner)))
                  (let [results (doall (for [claim claims]
                                         (evaluate-claim
                                          run claim (:datadir props) (:git props)
                                          (:recordsdir props) (:nthreads options))))]
                    (if (every? identity results)
                      (println "All claims verified.")
                      (println "Some claims not verified."))))))
            (System/exit 0))
          
          (= (:action options) "run")
          (let [problem (choose-problem (extract-problem (:params options)))
                git-dirty? (not-empty
                            (filter #(not= "??" (if (>= 2 (count %)) "??" (subs % 0 2)))
                               (split-lines (:out (sh (:git props) "status" "--porcelain")))))]
            (when (and (:upload options) git-dirty?)
              (println "Project has uncommitted changes. Commit with git before"
                       "running simulations.")
              (System/exit -1))
            (dosync
             (alter state/batch (constantly true))
             (alter state/problem (constantly problem)))
            (run-with-new-record run (:params options) (:datadir props) (:seed options)
              (:git props) (:recordsdir props) (:nthreads options) (:repetitions options)
              (:upload options) (:save-record options) false))
          
          :else
          (println "No action given."))))
