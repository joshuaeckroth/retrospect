(ns simulator.onerun
  (:require [simulator logs hypotheses])
  (:import [simulator.logs LogEntry AbducerLogEntry HypLogEntry])
  (:import [simulator.hypotheses HypothesisSpace])
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
          find-least-confident-decision
          count-branches]])
  (:use [simulator.strategies.composite :only [strategy-funcs]])
  (:use [simulator.sensors :only (update-sensors)])
  (:use [simulator.confidences])
  (:use [clojure.set]))

(def *meta* false)

(defrecord OneRunState
    [strategy
     meta-abduce                          ;; use meta abduction?
     resources
     results
     sensors
     ep-state-tree
     ep-state])

(defn init-one-run-state
  [strategy sensors problem-data meta?]
  (let [ep-state-tree (init-ep-state-tree problem-data)]
    (OneRunState. strategy meta?
                  {:compute 0 :milliseconds 0 :memory 0}
                  []
                  sensors
                  ep-state-tree (current-ep-state ep-state-tree))))

(defn init-one-run-states
  [strategies sensors problem-data]
  (doall (for [s strategies
               meta-abduce [true false]]
           (init-one-run-state s sensors problem-data meta-abduce))))

(defn need-meta-abduction?
  [ep-state-tree ep-state]
  (let [least-conf (find-least-confident-decision ep-state-tree)]
    (cond (not *meta*) false

          ;; is the present decision confidence worse than NEUTRAL
          ;; and the least confident past decision worse than NEUTRAL
          ;; and the number of existing branches not too large?
          (and (> NEUTRAL
                  (measure-decision-confidence ep-state))
               (> NEUTRAL
                  (:confidence (:decision least-conf)))
               (> 5 (count-branches ep-state-tree least-conf)))
          true

          :else false)))

(defn prepare-meta-abduce
  [or-state ep-state]
  "XXX: Note that presently no check is made for how low conf is
   least-conf; perhaps least-conf has high confidence... what would
   this imply?"
  (let [least-conf (find-least-confident-decision (:ep-state-tree or-state))
        branched-ep-state-tree (new-branch-ep-state
                                (:ep-state-tree or-state)
                                ep-state least-conf)]
    (-> or-state
        (assoc :ep-state-tree branched-ep-state-tree)
        (assoc :ep-state (current-ep-state branched-ep-state-tree)))))

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
                      :MetaAbduce (if (:meta-abduce or-state) "true" "false")
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
  (binding [*meta* (and (< 0 (:time (:ep-state or-state)))
                        (:meta-abduce or-state))]
    (let [start-time (. System (nanoTime))
          ep-state (explain-recursive
                    (:ep-state or-state)
                    (get strategy-funcs (:strategy or-state)))
          milliseconds (+ (:milliseconds (:resources or-state))
                          (/ (- (. System (nanoTime)) start-time)
                             1000000.0))
          ors (update-in or-state [:resources] assoc :milliseconds milliseconds)]
      (if (need-meta-abduction? (:ep-state-tree ors) ep-state)
        (explain (prepare-meta-abduce ors ep-state))
        (proceed-one-run-state ors ep-state)))))

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

