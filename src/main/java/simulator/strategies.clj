(ns simulator.strategies
  (:require [simulator logs hypotheses])
  (:import [simulator.logs LogEntry AbducerLogEntry HypLogEntry])
  (:import [simulator.hypotheses HypothesisSpace])
  (:use [simulator.epistemicstates :only
         (init-ep-state-tree current-ep-state update-ep-state-tree
                             new-child-ep-state
                             new-branch-ep-state
                             guess smartguess
                             essentials best smartbest
                             unexplained-helper
                             print-ep-state-tree measure-decision-confidence
                             find-least-confident-decision)])
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

                                        ;(def strategies (sort (keys strategy-funcs)))

(def strategies ["guess" "es-guess"])

(defrecord StrategyState
    [strategy
     meta-abduce                          ;; use meta abduction?
     resources
     truedata
     sensors
     ep-state-tree
     ep-state])

(defn init-strat-states
  [strategies truedata sensors problem-data]
  (doall (for [s strategies
               meta-abduce [true false]]
           (let [ep-state-tree (init-ep-state-tree problem-data)]
             (StrategyState. s meta-abduce
                             {:compute 0 :milliseconds 0 :memory 0}
                             truedata sensors
                             ep-state-tree (current-ep-state ep-state-tree))))))

(defn update-strat-state
  [strat-state ep-state sensors]
  (let [ep-state-tree (update-ep-state-tree (:ep-state-tree strat-state) ep-state)]
    (-> strat-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree))
        (assoc :sensors sensors))))

(defn proceed-strat-state
  [strat-state ep-state]
  (let [ep-state-tree (new-child-ep-state (:ep-state-tree strat-state) ep-state)]
    (-> strat-state
        (assoc :ep-state-tree ep-state-tree)
        (assoc :ep-state (current-ep-state ep-state-tree)))))

(defn need-meta-abduction?
  [ep-state-tree ep-state]
  (cond (not *meta*) false

        ;; is the present decision confidence worse than NEUTRAL
        ;; and the least confident past decision is worse than NEUTRAL?
        (and (> NEUTRAL
                (measure-decision-confidence ep-state))
             (> NEUTRAL
                (:confidence (:decision (find-least-confident-decision ep-state-tree)))))
        true

        :else false))

(defn prepare-meta-abduce
  [strat-state ep-state]
  "XXX: Note that presently no check is made for how low conf is
   least-conf; perhaps least-conf has high confidence... what would
   this imply?"
  (let [least-conf (find-least-confident-decision (:ep-state-tree strat-state))
        branched-ep-state-tree (new-branch-ep-state
                                (:ep-state-tree strat-state)
                                ep-state least-conf)]
    (-> strat-state
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

(defn explain
  [strat-state]
  (print-ep-state-tree (:ep-state-tree strat-state))
  (binding [*meta* (and (< 0 (:time (:ep-state strat-state)))
                        (:meta-abduce strat-state))]
    (let [start-time (. System (nanoTime))
          ep-state (explain-recursive
                    (:ep-state strat-state)
                    (get strategy-funcs (:strategy strat-state)))
          milliseconds (+ (:milliseconds (:resources strat-state))
                          (/ (- (. System (nanoTime)) start-time)
                             1000000.0))
          ss (update-in strat-state [:resources] assoc :milliseconds milliseconds)]
      (if (need-meta-abduction? (:ep-state-tree ss) ep-state)
        (explain (prepare-meta-abduce ss ep-state))
        (proceed-strat-state ss ep-state)))))

(defn general-evaluation
  [strat-state params start-time]
  (assoc params
    :Milliseconds (/ (double (- (. System (nanoTime)) start-time)) 1000000.0)
    :Strategy (:strategy strat-state)
    :MetaAbduce (if (:meta-abduce strat-state) "true" "false")
    :StrategyCompute (:compute (:resources strat-state))
    :StrategyMilliseconds (:milliseconds (:resources strat-state))
    :StrategyMemory (:memory (:resources strat-state))))

(defn run-simulation
  [problem strat-state params]
  (let [startTime (. System (nanoTime))]
    (loop [time 0
           ss strat-state]
      (if (> time (:Steps params))
        (merge (general-evaluation ss params startTime)
               ((:evaluate-fn problem) ss params))
        (let [sensors (update-sensors (:sensors ss) (get (:truedata ss) time) time)
              ep-state ((:runner-fn problem) (:ep-state ss) sensors params)
              ss2 (update-strat-state ss ep-state sensors)
              ss3 (explain ss2)
              newtime (:time (:ep-state ss3))]
          (recur newtime ss3))))))
