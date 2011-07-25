(ns retrospect.onerun
  (:use [retrospect.epistemicstates :only
         [init-ep-state-tree
          current-ep-state
          update-ep-state-tree
          new-child-ep-state]]))

(defn init-one-run-state
  [meta-strategy lazy sensors problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    {:meta-strategy meta-strategy
     :lazy lazy
     :meta-workspaces {}
     :original-problem-data problem-data
     :resources {:meta-activations 0 :explain-cycles 0 :milliseconds 0 :memory 0}
     :results [] :sensors sensors
     :ep-state-tree ep-state-tree
     :ep-state (current-ep-state ep-state-tree)}))

(defn init-one-run-states
  [options sensors problem-data]
  (for [meta-strategy (:MetaStrategy options)
        lazy (:Lazy options)]
    (init-one-run-state meta-strategy lazy sensors problem-data)))

(defn update-one-run-state
  [or-state ep-state]
  (let [ep-state-tree (update-ep-state-tree (:ep-state-tree or-state) ep-state)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))

(defn proceed-one-run-state
  [or-state ep-state time-now problem]
  (let [explain-cycles (:explain-cycles (:resources (:workspace ep-state)))
        ep-state-tree (new-child-ep-state (:ep-state-tree or-state)
                                          ep-state time-now problem)]
    (-> or-state
      (update-in [:resources :explain-cycles] + explain-cycles)
      (assoc :ep-state-tree ep-state-tree)
      (assoc :ep-state (current-ep-state ep-state-tree)))))
