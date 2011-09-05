(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.onerun :only
         [init-one-run-state update-one-run-state proceed-one-run-state]])
  (:use [retrospect.epistemicstates :only
         [explain current-ep-state]])
  (:use [retrospect.meta.reason :only
         [metareasoning-activated? metareason]])
  (:use [retrospect.evaluate :only [evaluate evaluate-comparative]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [set-seed my-rand-int]]))

(defn proceed-n-steps
  [n steps time truedata or-state]
  (loop [t time
         ors or-state]
    (if (or (> t steps) (= t (+ n time))) ors
        (recur (inc t) (update-in ors [:sensors] update-sensors (get truedata t) t)))))

(defn hypothesize
  [problem or-state time-now params]
  (let [[ep resources] ((:hypothesize-fn problem) (:ep-state or-state)
                          (:sensors or-state) time-now params)]
    (update-one-run-state or-state ep resources)))

(defn run-simulation-step
  [problem truedata or-state params monitor? player?]
  (let [time (:time (:ep-state or-state))
        ors-sensors (proceed-n-steps (:StepsBetween params) (:Steps params)
                                     time truedata or-state)
        time-now (min (dec (:Steps params)) (+ (dec (:StepsBetween params)) time))
        ;; start the clock
        start-time (. System (nanoTime))
        ors-hyps (hypothesize problem ors-sensors time-now params)
        ep-state (:ep-state ors-hyps)
        ep-explained (explain ep-state (:get-more-hyps-fn problem)
                              (:inconsistent-fn problem)
                              (double (/ (:Threshold params) 100.0)) params)
        ors-expl (update-one-run-state ors-hyps ep-explained {:compute 0 :memory 0})
        ors-committed (proceed-one-run-state ors-hyps ep-explained time-now problem)
        ors-meta (if (metareasoning-activated? ors-expl params)
                   (metareason problem ors-expl params) ors-expl)
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-next (proceed-one-run-state
                   ors-meta (current-ep-state (:ep-state-tree ors-meta))
                   time-now problem)
        ors-resources (update-in ors-next [:resources] assoc :milliseconds ms)
        ors-results (evaluate problem truedata ors-resources params)]
    (if (and (not player?) monitor?)
      ((:monitor-fn problem) problem truedata (:sensors ors-results)
       ors-results params)
      ors-results)))

(defn run-simulation
  [problem truedata or-state params monitor?]
  (loop [ors or-state]
    (when (nil? ors)
      (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= (:time (:ep-state ors)) (:Steps params))
      (last (:results ors))
      (recur (run-simulation-step problem truedata ors params monitor? false)))))

(defn extract-strategy
  [strategy]
  (let [features (split strategy #",")]
    (apply merge (map (fn [feature] (if (= "!" (subs feature 0 1))
                                      {(keyword (subs feature 1)) nil}
                                      {(keyword feature) true}))
                      features))))

(defn run
  [problem monitor? datadir params]
  (println "Running" params)
  (set-seed (:Seed params))
  (let [truedata ((:truedata-fn problem) datadir params)
        sensors ((:sensor-gen-fn problem) params)
        problem-data ((:gen-problem-data-fn problem) sensors datadir params)
        control-strategy (extract-strategy (:Control params))
        comparison-strategy (extract-strategy (:Comparison params))
        control-or-state (init-one-run-state control-strategy sensors problem-data)
        comparison-or-state (init-one-run-state comparison-strategy sensors problem-data)
        control-result
        (binding [last-id 0]
          (println "Control:" (:Control params))
          (run-simulation problem truedata control-or-state params monitor?))
        comparison-result
        (binding [last-id 0]
          (println "Comparison:" (:Comparison params))
          (run-simulation problem truedata comparison-or-state params monitor?))]
    [control-result comparison-result
     (evaluate-comparative problem control-result comparison-result params)]))

(defrecord Problem
  [name monitor-fn player-fns truedata-fn sensor-gen-fn prepared-map
   hypothesize-fn get-more-hyps-fn commit-decision-fn
   gen-problem-data-fn inconsistent-fn evaluate-fn evaluate-comparative-fn])
