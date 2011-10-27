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
  (:use [retrospect.random :only [set-seed my-rand-int]])
  (:use [retrospect.state]))

(defn proceed-n-steps
  [n steps time truedata or-state]
  (loop [t time
         ors or-state]
    (if (or (> t steps) (= t (+ n time))) ors
        (recur (inc t) (update-in ors [:sensors] update-sensors (get truedata t) t)))))

(defn hypothesize
  [or-state time-now]
  (let [[ep resources] ((:hypothesize-fn @problem) (:ep-state or-state)
                          (:sensors or-state) time-now)]
    (update-one-run-state or-state ep resources)))

(defn run-simulation-step
  [truedata or-state monitor? player?]
  (let [time (:time (:ep-state or-state))
        ors-sensors (proceed-n-steps (:StepsBetween params) (:Steps params)
                                     time truedata or-state)
        time-now (min (dec (:Steps params)) (+ (dec (:StepsBetween params)) time))
        ;; start the clock
        start-time (. System (nanoTime))
        ors-hyps (hypothesize ors-sensors time-now)
        ep-state (:ep-state ors-hyps)
        ep-explained (explain ep-state)
        ors-expl (update-one-run-state ors-hyps ep-explained {:compute 0 :memory 0})
        ors-meta (if (metareasoning-activated? ors-expl)
                   (metareason ors-expl) ors-expl)
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-next (proceed-one-run-state
                   ors-meta (current-ep-state (:ep-state-tree ors-meta))
                   time-now)
        ors-resources (update-in ors-next [:resources] assoc :milliseconds ms)
        ors-results (evaluate truedata ors-resources)]
    (if (and (not player?) monitor?)
      ((:monitor-fn @problem) truedata (:sensors ors-results) ors-results)
      ors-results)))

(defn run-simulation
  [truedata or-state monitor?]
  (loop [ors or-state]
    (when (nil? ors)
      (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= (:time (:ep-state ors)) (:Steps params))
      (last (:results ors))
      (recur (run-simulation-step truedata ors monitor? false)))))

(defn run
  [comparative? monitor? params]
  (if comparative?
    ;; if comparative, run two simulations
    (let [[control-params comparison-params] params
          control-result
          (binding [last-id 0
                    params control-params]
            (set-seed (:Seed control-params))
            (let [control-truedata ((:truedata-fn @problem))
                  control-sensors ((:sensor-gen-fn @problem))
                  control-problem-data ((:gen-problem-data-fn @problem) control-sensors)
                  control-or-state (init-one-run-state control-sensors control-problem-data)]
              (println "Control:" control-params)
              (assoc (run-simulation control-truedata control-or-state monitor?)
                :control-params (pr-str control-params) :comparison-params (pr-str comparison-params))))
          comparison-result
          (binding [last-id 0
                    params comparison-params]
            (set-seed (:Seed comparison-params))
            (let [comparison-truedata ((:truedata-fn @problem))
                  comparison-sensors ((:sensor-gen-fn @problem))
                  comparison-problem-data ((:gen-problem-data-fn @problem) comparison-sensors)
                  comparison-or-state (init-one-run-state comparison-sensors comparison-problem-data)]
              (println "Comparison:" comparison-params)
              (assoc (run-simulation comparison-truedata comparison-or-state monitor?)
                :control-params (pr-str control-params) :comparison-params (pr-str comparison-params))))]
      [control-result comparison-result
       (assoc (evaluate-comparative control-result comparison-result control-params comparison-params)
         :control-params (first params) :comparison-params (second params))])
    ;; if non-comparative, just run the simulation
    (binding [last-id 0
              params params]
      (set-seed (:Seed params))
      (let [truedata ((:truedata-fn @problem))
            sensors ((:sensor-gen-fn @problem))
            problem-data ((:gen-problem-data-fn @problem) sensors)
            or-state (init-one-run-state sensors problem-data)]
        (println "Params:" params)
        (assoc (run-simulation truedata or-state monitor?)
          :params (pr-str params))))))

(defrecord Problem
  [name monitor-fn player-fns truedata-fn sensor-gen-fn prepared-map
   hypothesize-fn get-more-hyps-fn commit-decision-fn
   gen-problem-data-fn inconsistent-fn evaluate-fn evaluate-comparative-fn
   default-params])
