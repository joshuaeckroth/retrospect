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
          measure-decision-confidence
          count-branches]])
  (:use [simulator.strategies.composite :only [strategy-funcs]])
  (:use [simulator.strategies.metastrategies :only [meta-strategy-funcs]])
  (:use [simulator.sensors :only (update-sensors)])
  (:use [simulator.confidences])
  (:use [clojure.set]))

(defrecord OneRunState
    [strategy
     meta-strategy
     meta-log
     resources
     results
     sensors
     ep-state-tree
     ep-state])

(defn init-one-run-state
  [strategy meta-strategy sensors problem-data]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    (OneRunState. strategy meta-strategy []
                  {:compute 0 :milliseconds 0 :memory 0}
                  []
                  sensors
                  ep-state-tree (current-ep-state ep-state-tree))))

(defn init-one-run-states
  [strategies meta-strategies sensors problem-data]
  (doall (for [s strategies ms meta-strategies]
           (init-one-run-state s ms sensors problem-data))))

(defn attempt-meta-abduction
  [or-state]
  "Return new or-state if there is a new one."
  (if-let [ors ((get meta-strategy-funcs (:meta-strategy or-state)) or-state)]
    ors))

(defn explain-recursive
  [ep-state funcs]
  (loop [fs funcs
         ep ep-state]
    (if (empty? fs) ep
        (let [ep2 ((first fs) ep)]
          (if ep2
            (recur fs ep2)
            (recur (rest fs) ep))))))

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
                      :StrategyCompute (:compute (:resources or-state))
                      :StrategyMilliseconds (:milliseconds (:resources or-state))
                      :StrategyMemory (:memory (:resources or-state))))))

(defn explain
  [or-state]
  "Takes a OneRunState with sensors that have sensed and
   an epistemic state that has the problem's hypotheses.
   Returns a OneRunState in which the current epistemic state
   has accepted a decision (possibly after a few steps of
   meta-abduction)."
  (let [start-time (. System (nanoTime))
        ep-state (explain-recursive
                  (:ep-state or-state)
                  (get strategy-funcs (:strategy or-state)))
        milliseconds (+ (:milliseconds (:resources or-state))
                        (/ (- (. System (nanoTime)) start-time)
                           1000000.0))
        ors (update-in (update-one-run-state or-state ep-state)
                       [:resources] assoc :milliseconds milliseconds)]
    (if-let [ors2 (attempt-meta-abduction ors)]
      (explain ors2)
      (proceed-one-run-state ors ep-state))))

(defn run-simulation-step
  [problem truedata or-state params start-time]
  "Updates the OneRunState so that the sensors have sensed the events
   at the current time step, and the problem has generated hypotheses;
   then runs the abduction machinery (the (explain) function), next
   updates the problem data based on the new (post-decision) epistemic
   state, and returns a OneRunState in which the accepted decision has
   been evaluated."
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

