(ns retrospect.repl
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]])
  (:require [retrospect.state :as state])
  (:use [retrospect.epistemicstates :only
         [print-ep-state-tree previous-ep-state]]))

(defn tracking-player
  []
  (start-player tracking-problem :repl true))

(defn params
  []
  @state/params)

(defn eptree
  []
  (print-ep-state-tree (:ep-state-tree @state/or-state)))

(defn pdata
  []
  (if @state/or-state
    (if-let [ep-state (previous-ep-state (:ep-state-tree @state/or-state))]
      (:problem-data ep-state))))

(defn select-hyps
  [& ids]
  (if @state/or-state
    (if-let [ep-state (previous-ep-state (:ep-state-tree @state/or-state))]
      (filter (fn [h] (some #(re-matches % (:id h))
                            (map re-pattern ids)))
              (map :hyp (:added (:log (:workspace ep-state))))))))

