(ns retrospect.onerun
  (:use [retrospect.epistemicstates :only
         [init-ep-state-tree
          current-ep-state
          update-ep-state-tree
          new-child-ep-state]])
  (:use [retrospect.state]))

(defn init-one-run-state
  [sensors problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    {:meta-workspaces {}
     :original-problem-data problem-data
     :resources {:meta-activations 0 :meta-accepted 0
                 :explain-cycles 0 :hypothesis-count 0
                 :milliseconds 0 :compute 0 :memory 0}
     :results []
     :sensors sensors
     :ep-state-tree ep-state-tree
     :ep-state (current-ep-state ep-state-tree)}))

(defn clear-resources
  [or-state]
  (assoc or-state :resources {:meta-activations 0 :meta-accepted 0
                              :explain-cycles 0 :hypothesis-count 0
                              :milliseconds 0 :compute 0 :memory 0}))

(defn update-one-run-state
  [or-state ep-state {:keys [compute memory]}]
  (let [ep-state-tree (update-ep-state-tree (:ep-state-tree or-state) ep-state)]
    (-> or-state
      (assoc :ep-state-tree ep-state-tree)
      (assoc :ep-state (current-ep-state ep-state-tree))
      (update-in [:resources :compute] + compute)
      (update-in [:resources :memory] + memory))))

(defn proceed-one-run-state
  [or-state ep-state time-now]
  (let [explain-cycles (:explain-cycles (:resources (:workspace ep-state)))
        hypothesis-count (:hypothesis-count (:resources (:workspace ep-state)))
        ep-state-tree (new-child-ep-state (:ep-state-tree or-state) ep-state time-now)]
    (-> or-state
      (update-in [:resources :explain-cycles] + explain-cycles)
      (update-in [:resources :hypothesis-count] + hypothesis-count)
      (assoc :ep-state-tree ep-state-tree)
      (assoc :ep-state (current-ep-state ep-state-tree)))))
