(ns retrospect.core
  (:gen-class)
  (:import (javax.swing SwingUtilities))
  (:require [clojure.string :as str])
  (:use [clojure.java.shell :only [sh]])
  (:use [clojure.tools.cli :only [cli]])
  (:use [geppetto.random :only [rgen new-seed]])
  (:require [retrospect.state :as state])
  (:use propertea.core)
  (:use [clojure.pprint :only [pprint]])
  (:use [geppetto.misc])
  (:use [geppetto.parameters :only [read-params extract-problem]])
  (:use [geppetto.runs :only [get-run]])
  (:use [geppetto.claim])
  (:use [geppetto.repeat])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.aerial.problem :only [aerial-problem]])
  #_(:use [retrospect.problems.words.problem :only [words-problem]])
  #_(:use [retrospect.problems.classify.problem :only [classify-problem]])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.simulate :only [run]])
  (:use [geppetto.records :only [run-with-new-record submit-results]])
  (:use [geppetto.optimize :only [optimize]])
  (:use [retrospect.player :only [start-player]]))

(comment
  (or (= "Words" problem) (= "words" problem))
  words-problem
  (or (= "Classify" problem) (= "classify" problem))
  classify-problem)

(defn choose-problem
  [problem]
  (cond (or (= "Tracking" problem) (= "tracking" problem))
        tracking-problem
        (or (= "Aerial" problem) (= "aerial" problem))
        aerial-problem
        (or (= "AbdExp" problem) (= "abdexp" problem))
        abdexp-problem))

(defn choose-reasoner
  [reasoner]
  (cond (or (= "Abduction" reasoner) (= "abduction" reasoner))
        reason-abduction))

(defn -main [& args]
  (let [[options _ banner]
        (cli args
             ["--action" "Action (run/player/optimize/resubmit/verify-claims)" :default "player"]
             ["--reasoner" "Reasoning algorithm" :default "abduction"]
             ["--problem" "Problem" :default "tracking"]
             ["--params" "Parameters identifier (e.g. 'Words/foobar')" :default ""]
             ["--runid" "Run ID for repeating" :default "" :parse-fn #(Integer. %)]
             ["--claims" "Which claims to verify, comma separated (default: all)"
              :default nil :parse-fn #(set (str/split % #"\s*,\s*"))]
             ["--nthreads" "Number of threads" :default 1 :parse-fn #(Integer. %)]
             ["--repetitions" "Number of repetitions" :default 10 :parse-fn #(Integer. %)]
             ["--seed" "Seed" :default 0 :parse-fn #(Integer. %)]
             ["--upload" "Upload?" :default true :parse-fn #(= "true" %)]
             ["--save-record" "Save in record directory?" :default true :parse-fn #(= "true" %)]
             ["--recdir" "Record directory (to resubmit)" :default ""]
             ["--log" "Show verbose logging?" :default false :parse-fn #(= "true" %)]
             ["--quiet" "Quiet mode (hide progress messages)?" :default false :parse-fn #(= "true" %)]
             ["--opt-metric" "Optimize metric" :parse-fn keyword]
             ["--opt-min-or-max" "Optimize to 'min' or 'max' of metric" :default :max :parse-fn keyword]
             ["--opt-alpha" "Optimize alpha (double)" :default 0.95 :parse-fn #(Double. %)]
             ["--opt-init-temp" "Optimize initial temperature (double)" :default 1 :parse-fn #(Double. %)]
             ["--opt-temp-sched" "Optimize temperature schedule (int)" :default 100 :parse-fn #(Integer. %)]
             ["--opt-stop-cond1" "Optimize stopping condition 1 (double)" :default 0.02 :parse-fn #(Double. %)]
             ["--opt-stop-cond2" "Optimize stopping condition 2 (int)" :default 5 :parse-fn #(Integer. %)])
        reasoner (choose-reasoner (:reasoner options))
        problem (choose-problem (:problem options))
        props (read-properties "config.properties")]
    (setup-geppetto (:geppetto_dbhost props)
                    (:geppetto_dbport props)
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
    (cond (and (= (:action options) "run") (= (:action options) "optimize") (= "" (:params options)))
          (println "--params identifier required.")
          
          (= (:action options) "player")
          ;; start the player on swing's "event dispatch thread"
          (SwingUtilities/invokeLater start-player)

          (= (:action options) "resubmit")
          (submit-results (:recdir options))

          (= (:action options) "verify-claims")
          (do
            (dosync (alter state/batch (constantly true)))
            (let [claims (let [cs (concat (get @state/problem :claims [])
                                          (get-in @state/problem [(:key reasoner) :claims] []))]
                           (if (:claims options)
                             (filter #((:claims options) (str (:name %))) cs)
                             cs))]
              (if (empty? claims)
                (println (format "No claims (or no valid claims selected) for %s" (:name @state/problem)))
                (do
                  (println (format "Verifying %d claims for %s with reasoner %s..."
                                   (count claims) (:name @state/problem) (:name @state/reasoner)))
                  (let [results (doall (for [claim claims]
                                         (evaluate-claim run claim (:datadir props) (:git props)
                                                         "/tmp" (:nthreads options))))]
                    (if (every? identity results)
                      (do (println "All claims verified.")
                          (System/exit 0))
                      (do (println "Some claims not verified.")
                          (System/exit -1)))))))
            (System/exit 0))

          (= (:action options) "repeat")
          (let [problem (choose-problem (:problem (get-run (:runid options))))]
            (dosync
             (alter state/problem (constantly problem))
             (alter state/batch (constantly true)))
            (let [identical? (repeat-run (:runid options) run (:datadir props) (:git props)
                                         (:recordsdir props) (:nthreads options))]))

          (= (:action options) "verify-identical")
          (let [problem (choose-problem (:problem (get-run (:runid options))))
                only-ignore {:control {:ignore [:Milliseconds]}
                             :comparison {:ignore [:Milliseconds]}
                             :comparative {:ignore [:Milliseconds]}}]
            (dosync
             (alter state/problem (constantly problem))
             (alter state/batch (constantly true)))
            (pprint (verify-identical-repeat-run
                     (:runid options) only-ignore run
                     (:datadir props) (:git props) (:nthreads options))))
          
          (= (:action options) "run")
          (let [problem (choose-problem (extract-problem (:params options)))
                git-dirty? (not-empty
                            (filter #(not= "??" (if (>= 2 (count %)) "??" (subs % 0 2)))
                                    (str/split-lines (:out (sh (:git props) "status" "--porcelain")))))]
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

          (= (:action options) "optimize")
          (let [problem (choose-problem (extract-problem (:params options)))
                git-dirty? (not-empty
                            (filter #(not= "??" (if (>= 2 (count %)) "??" (subs % 0 2)))
                                    (str/split-lines (:out (sh (:git props) "status" "--porcelain")))))]
            (when (and (:upload options) git-dirty?)
              (println "Project has uncommitted changes. Commit with git before"
                       "running simulations.")
              (System/exit -1))
            (dosync
             (alter state/batch (constantly true))
             (alter state/problem (constantly problem)))
            (optimize run (:params options) (:opt-min-or-max options) (:opt-metric options)
                      (:opt-alpha options) (:opt-init-temp options) (:opt-temp-sched options)
                      (:opt-stop-cond1 options) (:opt-stop-cond2 options)
                      (:datadir props) (:seed options) (:git props) (:recordsdir props) (:nthreads options)
                      (:repetitions options) (:upload options) (:save-record options)))
          
          :else
          (println "No action given."))))


