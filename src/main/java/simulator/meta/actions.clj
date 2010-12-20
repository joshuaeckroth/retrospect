(ns simulator.meta.actions
  (:use [simulator.epistemicstates :only
         [new-branch-ep-state current-ep-state update-ep-state-tree]]))

(defn do-nothing
  [or-state]
  or-state)

(defn switch-states
  [ep-state or-state])

(defn delete-least-conf
  [or-state])

(defn reset-decision
  [or-state])

(defn gen-more-problem-hyps
  [or-state])

