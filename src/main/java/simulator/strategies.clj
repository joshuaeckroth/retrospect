(ns simulator.strategies
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
          guess smartguess
          essentials best smartbest
          unexplained-helper
          print-ep-state-tree
          list-ep-states
          measure-decision-confidence
          find-least-confident-decision
          count-branches]])
  (:use [simulator.sensors :only (update-sensors)])
  (:use [simulator.confidences])
  (:use clojure.set))

(def *meta* false)

(def strategy-funcs
  {"guess" [guess]
   "smartguess" [smartguess]
   "es-guess" [essentials guess]
   "es-smartguess" [essentials smartguess]
   "es-b1-guess" [essentials (best 1) guess]
   "es-sb1-guess" [essentials (smartbest 1) guess]
   "es-b1-smartguess" [essentials (best 1) smartguess]
   "es-sb1-smartguess" [essentials (smartbest 1) smartguess]
   "es-b2-b1-guess" [essentials (best 2) (best 1) guess]
   "es-sb2-sb1-guess" [essentials (smartbest 2) (smartbest 1) guess]
   "es-b2-b1-smartguess" [essentials (best 2) (best 1) smartguess]
   "es-sb2-sb1-smartguess" [essentials (smartbest 2) (smartbest 1) smartguess]
   "es-b3-b2-b1-guess" [essentials (best 3) (best 2) (best 1) guess]
   "es-sb3-sb2-sb1-guess" [essentials (smartbest 3) (smartbest 2) (smartbest 1) guess]
   "es-b3-b2-b1-smartguess" [essentials (best 3) (best 2) (best 1) smartguess]
   "es-sb3-sb2-sb1-smartguess" [essentials (smartbest 3) (smartbest 2)
                                (smartbest 1) smartguess]
   "es-b4-b3-b2-b1-smartguess" [essentials (best 4) (best 3) (best 2) (best 1) smartguess]
   "es-sb4-sb3-sb2-sb1-smartguess" [essentials (smartbest 4) (smartbest 3) (smartbest 2)
                                (smartbest 1) smartguess]
   "es" [essentials]
   "es-b1" [essentials (best 1)]
   "es-sb1" [essentials (smartbest 1)]
   "es-b2-b1" [essentials (best 2) (best 1)]
   "es-sb2-sb1" [essentials (smartbest 2) (smartbest 1)]
   "es-b3-b2-b1" [essentials (best 3) (best 2) (best 1)]
   "es-sb3-sb2-sb1" [essentials (smartbest 3) (smartbest 2) (smartbest 1)]
   "es-sb4-sb3-sb2-sb1" [essentials (smartbest 4) (smartbest 3)
                         (smartbest 2) (smartbest 1)]})

(def strategies (sort (keys strategy-funcs)))

;;(def strategies ["es-sb4-sb3-sb2-sb1-smartguess"])

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
  [or-state ep-state sensors]
  (let [ep-state-tree (update-ep-state-tree (:ep-state-tree or-state) ep-state)]
    (-> or-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree))
        (assoc :sensors sensors))))

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
                     (previous-ep-state (:ep-state-tree or-state))
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
  (let [time (:time (:ep-state or-state))
        sensors (update-sensors (:sensors or-state) (get truedata time) time)
        ep-state ((:runner-fn problem) (:ep-state or-state) sensors params)
        ors2 (update-one-run-state or-state ep-state sensors)
        ors3 (explain ors2)]
    (evaluate problem truedata ors3 params start-time)))

(defn run-simulation
  [problem truedata or-state params]
  (let [start-time (. System (nanoTime))]
    (loop [ors or-state]
      (if (> (:time (:ep-state ors)) (:Steps params)) (:results ors)
          (recur (run-simulation-step problem truedata ors params start-time))))))

