(ns simulator.onerun
  (:use [simulator.epistemicstates :only
         [init-ep-state-tree
          current-ep-state
          previous-ep-state
          update-ep-state-tree
          new-child-ep-state
          new-branch-ep-state
          print-ep-state-tree
          list-ep-states
          count-branches]])
  (:use [simulator.meta.hypotheses :only
         [generate-meta-hypotheses]])
  (:use [simulator.workspaces :only
         [init-workspace lookup-hyps explain delete-ancient-hyps]])
  (:use [simulator.sensors :only [update-sensors]])
  (:use [simulator.confidences])
  (:use [clojure.set]))

(defrecord OneRunState
    [meta-abduction
     meta-log
     resources
     results
     sensors
     ep-state-tree
     ep-state])

(defn init-one-run-state
  [meta-abduction sensors problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    (OneRunState. meta-abduction []
                  {:meta-abductions 0 :compute 0 :milliseconds 0 :memory 0}
                  []
                  sensors
                  ep-state-tree (current-ep-state ep-state-tree))))

(defn init-one-run-states
  [options sensors problem-data]
  (doall (for [meta-abduction (:MetaAbduction options)]
           (init-one-run-state meta-abduction sensors problem-data))))

(defn update-one-run-state
  ([or-state ep-state]
     (let [ep-state-tree (update-ep-state-tree (:ep-state-tree or-state) ep-state)]
       (-> or-state
           (assoc :ep-state-tree ep-state-tree)
           (assoc :ep-state (current-ep-state ep-state-tree)))))
  ([or-state ep-state sensors]
     (assoc (update-one-run-state or-state ep-state) :sensors sensors)))

(defn proceed-one-run-state
  [or-state ep-state]
  (let [ep-state-tree (new-child-ep-state (:ep-state-tree or-state) ep-state)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))

(defn evaluate
  [problem truedata or-state params start-time]
  (update-in or-state [:results] conj
             (merge ((:evaluate-fn problem)
                     (:ep-state or-state)
                     (:sensors or-state) truedata params)
                    (assoc params
                      :Milliseconds (/ (double (- (. System (nanoTime)) start-time))
                                       1000000.0)
                      :MetaAbduction (:meta-abduction or-state)
                      :MetaAbductions (:meta-abductions (:resources or-state))
                      :ExplainCompute (:compute (:resources or-state))
                      :ExplainMilliseconds (:milliseconds (:resources or-state))
                      :ExplainMemory (:memory (:resources or-state))))))

(defn do-explain-meta
  [or-state]
  (if (not (:meta-abduction or-state)) or-state
      (let [workspace (-> (init-workspace)
                          (generate-meta-hypotheses or-state)
                          (explain))
            ors (update-in or-state [:meta-log] conj (:abducer-log workspace))]
        (reduce (fn [ors action] (action ors)) ors
                (map :update-fn (lookup-hyps workspace (:accepted (:decision workspace))))))))

(defn do-explain
  "Takes a OneRunState with sensors that have sensed and
   an epistemic state that has the problem's hypotheses.
   Returns a OneRunState in which the current epistemic state
   has accepted a decision (possibly after a few steps of
   meta-abduction)."
  [or-state]
  (let [start-time (. System (nanoTime))
        workspace (explain (delete-ancient-hyps (:workspace (:ep-state or-state))
                                                (:time (:ep-state or-state))))
        ep-state (assoc (:ep-state or-state) :workspace workspace)
        ep-state-updated (doall
                          (reduce (fn [ep action]
                                    (update-in ep [:problem-data] action))
                                  ep-state
                                  (map :update-fn
                                       (lookup-hyps workspace
                                                    (:accepted (:decision workspace))))))
        milliseconds (+ (:milliseconds (:resources or-state))
                        (/ (- (. System (nanoTime)) start-time)
                           1000000.0))
        ors (update-in (update-one-run-state or-state ep-state-updated)
                       [:resources] assoc :milliseconds milliseconds)
        ors-meta (do-explain-meta ors)
        ep-state-meta (:ep-state ors-meta)]
    (proceed-one-run-state ors-meta ep-state-meta)))

(defn run-simulation-step
  "Updates the OneRunState so that the sensors have sensed the events
   at the current time step, and the problem has generated hypotheses;
   then runs the abduction machinery (the (explain) function), next
   updates the problem data based on the new (post-decision) epistemic
   state, and returns a OneRunState in which the accepted decision has
   been evaluated."
  [problem truedata or-state params start-time]
  (let [time (:time (:ep-state or-state))
        sensors (update-sensors (:sensors or-state) (get truedata time) time)
        ep-state ((:gen-hyps-fn problem) (:ep-state or-state) sensors params)
        ors (do-explain (update-one-run-state or-state ep-state sensors))]
    (evaluate problem truedata ors params start-time)))

(defn run-simulation
  [problem truedata or-state params]
  (let [start-time (. System (nanoTime))]
    (loop [ors or-state]
      (if (>= (:time (:ep-state ors)) (:Steps params)) (:results ors)
          (recur (run-simulation-step problem truedata ors params start-time))))))

