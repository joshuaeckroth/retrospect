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
  (:use [simulator.strategies.composite :only [strategy-info]])
  (:use [simulator.strategies.explain :only [explain-recursive]])
  (:use [simulator.meta.hypotheses :only
         [generate-meta-hypotheses]])
  (:use [simulator.workspaces :only [init-workspace]])
  (:use [simulator.sensors :only [update-sensors]])
  (:use [simulator.confidences])
  (:use [clojure.set]))

(defrecord OneRunState
    [strategy
     meta-strategy
     operative-strategy
     operative-meta-strategy
     meta-log
     resources
     results
     sensors
     ep-state-tree
     ep-state])

(defn init-one-run-state
  [strategy meta-strategy sensors problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    (OneRunState. strategy meta-strategy strategy meta-strategy []
                  {:meta-abductions 0 :compute 0 :milliseconds 0 :memory 0}
                  []
                  sensors
                  ep-state-tree (current-ep-state ep-state-tree))))

(defn init-one-run-states
  [strategies sensors problem-data]
  (doall (for [s strategies ms strategies]
           (init-one-run-state s ms sensors problem-data))))

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
                      :Strategy (:strategy or-state)
                      :MetaStrategy (:meta-strategy or-state)
                      :MetaAbductions (:meta-abductions (:resources or-state))
                      :StrategyCompute (:compute (:resources or-state))
                      :StrategyMilliseconds (:milliseconds (:resources or-state))
                      :StrategyMemory (:memory (:resources or-state))))))

(defn explain-meta
  [or-state]
  (let [workspace (-> (init-workspace)
                      (generate-meta-hypotheses or-state)
                      (explain-recursive
                       (:funcs (get strategy-info (:operative-meta-strategy or-state)))))
        ors (update-in or-state [:meta-log] conj (:abducer-log workspace))]
    (reduce (fn [ors action] (action ors)) ors
            (map :action (:hyps (:decision workspace))))))

(defn explain
  "Takes a OneRunState with sensors that have sensed and
   an epistemic state that has the problem's hypotheses.
   Returns a OneRunState in which the current epistemic state
   has accepted a decision (possibly after a few steps of
   meta-abduction)."
  [or-state]
  (let [start-time (. System (nanoTime))
        workspace (explain-recursive
                   (:workspace (:ep-state or-state))
                   (:funcs (get strategy-info (:operative-strategy or-state))))
        ep-state (assoc (:ep-state or-state) :workspace workspace)
        milliseconds (+ (:milliseconds (:resources or-state))
                        (/ (- (. System (nanoTime)) start-time)
                           1000000.0))
        ors (update-in (update-one-run-state or-state ep-state)
                       [:resources] assoc :milliseconds milliseconds)
        ors-meta (explain-meta ors)
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
        ors (explain (update-one-run-state or-state ep-state sensors))
        ors2 (update-one-run-state
              ors ((:update-problem-data-fn problem) (:ep-state ors)))]
    (evaluate problem truedata ors2 params start-time)))

(defn run-simulation
  [problem truedata or-state params]
  (let [start-time (. System (nanoTime))]
    (loop [ors or-state]
      (if (>= (:time (:ep-state ors)) (:Steps params)) (:results ors)
          (recur (run-simulation-step problem truedata ors params start-time))))))

