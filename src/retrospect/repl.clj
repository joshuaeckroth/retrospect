(ns retrospect.repl
  (:import (misc AlphanumComparator))
  (:require [clojure.string :as str])
  (:require [retrospect.reason.abduction.gui.logs :as logs])
  (:require [retrospect.reason.abduction.workspace :as workspace])
  (:require [retrospect.problems.tracking.hypotheses :as track-hyps])
  (:use [geppetto.random])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-problem-log player-get-truedata-log]])
  (:use [retrospect.problems.words.problem :only [words-problem]])
  (:use [retrospect.epistemicstates :only [cur-ep prev-ep]])
  (:use [retrospect.state])
  (:use [retrospect.simulate :only
         [run-simulation-step get-default-params init-ors]]))

(defn setup-tracking
  []
  (dosync
   (alter datadir (constantly "data"))
   (alter database (constantly "http://retrospect.artifice.cc:5984"))
   (alter reason (constantly reason-abduction))
   (alter problem (constantly tracking-problem)))
  (change-params)
  (alter-var-root (var rgen) (constantly (new-seed 10)))
  nil)

(defn tracking-believed
  []
  (println (player-get-problem-log)))

(defn tracking-true
  []
  (println (player-get-truedata-log)))

(defn setup-words
  []
  (dosync
   (alter datadir (constantly "data"))
   (alter database (constantly "http://retrospect.artifice.cc:5984"))
   (alter reason (constantly reason-abduction))
   (alter problem (constantly words-problem)))
  (change-params)
  (alter-var-root (var rgen) (constantly (new-seed 10)))
  nil)

(defn change-params
  [& keyvals]
  (alter-var-root (var params)
                  (constantly (merge (get-default-params)
                                     (apply hash-map keyvals)))))

(defn new-simulation
  []
  (set-last-id 0)
  (dosync
   (alter truedata (constantly ((:generate-truedata-fn @problem))))
   (alter sensors (constantly ((:generate-sensors-fn @problem))))
   (alter results (constantly []))
   (alter or-state (constantly (init-ors @sensors (:training @truedata)))))
  nil)

(defn step
  []
  (let [ors (run-simulation-step @truedata @or-state true)
        time-n (:time (cur-ep (:est ors)))
        time-p (or (:time (prev-ep (:est ors))) -1)]
    (dosync (alter or-state (constantly ors))
            (alter time-now (constantly time-n))
            (alter time-prev (constantly time-p))
            (alter results (constantly (:results (cur-ep (:est ors))))))
    (last @results)))

(defn hypotheses
  [type]
  (let [hyps (get (:hypotheses (:workspace (cur-ep (:est @or-state)))) type)]
    (println (str/join "\n" (map str (sort-by :id (AlphanumComparator.) hyps))))))

(defn find-hyp-by-id
  [hypid]
  (let [ws (:workspace (cur-ep (:est @or-state)))]
    (first (filter #(= (:id %) hypid)
              (apply concat (vals (:hypotheses ws)))))))

(defn hyp-info
  [hypid]
  (let [ws (:workspace (cur-ep (:est @or-state)))]
    (println (logs/hyp-info ws @time-now (find-hyp-by-id hypid)))))

(defn show-cycle
  [i]
  (let [ws (:workspace (cur-ep (:est @or-state)))]
    (println (logs/build-cycle (:log ws) (dec i)))))

(defn find-conflicts
  [hypid]
  (let [ws (:workspace (cur-ep (:est @or-state)))]
    (println (workspace/find-conflicts ws (find-hyp-by-id hypid)))))





