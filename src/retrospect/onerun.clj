(ns retrospect.onerun
  (:use [retrospect.epistemicstates :only
         [init-ep-state-tree
          current-ep-state
          update-ep-state-tree
          new-child-ep-state]]))

(defn init-one-run-state
  [meta-abduction lazy sensors seed problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    {:meta-abduction meta-abduction :lazy lazy
     :meta-workspaces {}
     :resources {:meta-abductions 0 :compute 0 :milliseconds 0 :memory 0
                 :meta-accepted-bad 0 :meta-accepted-impossible 0
                 :meta-accepted-batch 0
                 :meta-accepted-impossible-lconf 0 :meta-accepted-none 0}
     :results [] :sensors sensors
     :seed seed
     :ep-state-tree ep-state-tree
     :ep-state (current-ep-state ep-state-tree)}))

(defn init-one-run-states
  [options sensors seed problem-data]
  (for [meta-abduction (:MetaAbduction options)
        lazy (:Lazy options)]
    (init-one-run-state meta-abduction lazy
                        sensors seed problem-data)))

(defn update-one-run-state
  [or-state ep-state]
  (let [ep-state-tree (update-ep-state-tree (:ep-state-tree or-state) ep-state)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))

(defn proceed-one-run-state
  [or-state ep-state time-now problem]
  (let [ep-state-tree (new-child-ep-state (:ep-state-tree or-state)
                                          ep-state time-now problem)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))
