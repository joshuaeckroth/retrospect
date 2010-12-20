(ns simulator.meta.actions
  (:use [simulator.epistemicstates :only
         [new-branch-ep-state current-ep-state update-ep-state-tree]]))

(defn do-nothing
  [or-state]
  or-state)

(defn switch-states
  [ep-state or-state])

(defn change-strategy
  [strategy workspace or-state]
  (let [
        ;; branch the tree
        branched-ep-state-tree (new-branch-ep-state
                                (:ep-state-tree or-state)
                                (:ep-state or-state)
                                (:ep-state or-state))

        ;; put the workspace & strategy into the new branched ep-state
        new-ep-state (assoc (current-ep-state branched-ep-state-tree)
                       :workspace workspace
                       :strategy strategy)

        ;; update the tree
        ep-state-tree (update-ep-state-tree branched-ep-state-tree new-ep-state)]

    ;; put the tree in the or-state
    (assoc or-state
      :ep-state-tree ep-state-tree
      :ep-state (current-ep-state ep-state-tree))))

(defn delete-least-conf
  [or-state])

(defn reset-decision
  [or-state])

(defn gen-more-problem-hyps
  [or-state])

