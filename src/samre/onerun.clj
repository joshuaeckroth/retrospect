(ns samre.onerun
  (:use [samre.epistemicstates :only
         [init-ep-state-tree
          current-ep-state
          update-ep-state-tree
          new-child-ep-state]]))

(defrecord OneRunState
    [meta-abduction
     lazy
     meta-log
     resources
     results
     sensors
     ep-state-tree
     ep-state])

(defn init-one-run-state
  [meta-abduction lazy sensors problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    (OneRunState. meta-abduction lazy []
                  {:meta-abductions 0 :compute 0 :milliseconds 0 :memory 0}
                  []
                  sensors
                  ep-state-tree (current-ep-state ep-state-tree))))

(defn init-one-run-states
  [options sensors problem-data]
  (doall (for [meta-abduction (:MetaAbduction options)
               lazy (:Lazy options)]
           (init-one-run-state meta-abduction lazy sensors problem-data))))

(defn update-one-run-state
  [or-state ep-state]
  (let [ep-state-tree (update-ep-state-tree (:ep-state-tree or-state) ep-state)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))

(defn proceed-one-run-state
  [or-state ep-state time-now problem params]
  (let [ep-state-tree (new-child-ep-state (:ep-state-tree or-state)
                                          ep-state time-now problem params)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))
