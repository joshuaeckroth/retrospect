(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.onerun :only
         [init-one-run-state update-one-run-state proceed-one-run-state
          clear-resources]])
  (:use [retrospect.epistemicstates :only
         [explain current-ep-state previous-ep-state]])
  (:use [retrospect.meta.reason :only
         [metareasoning-activated? metareason]])
  (:use [retrospect.evaluate :only [evaluate evaluate-comparative]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.state]))

(defn update-sensors-from-to
  [time time-now truedata or-state]
  (loop [t time
         ors or-state]
    (let [ors2 (update-in ors [:sensors] update-sensors truedata t)]
      (if (>= t time-now) ors2
          (recur (inc t) ors2)))))

(defn hypothesize
  [or-state time-now]
  (let [[ep resources] ((:hypothesize-fn @problem) (:ep-state or-state)
                          (:sensors or-state) time-now)]
    (update-one-run-state or-state ep resources)))

(defn run-simulation-step
  [truedata or-state monitor? player?]
  (let [time (:time (:ep-state or-state))
        time-now (min (:Steps params) (+ (:StepsBetween params) time))
        ors-clean (clear-resources or-state)
        ors-sensors (update-sensors-from-to time time-now truedata ors-clean)
        ;; start the clock
        start-time (. System (nanoTime))
        ors-hyps (hypothesize ors-sensors time-now)
        ep-state (:ep-state ors-hyps)
        ep-explained (explain ep-state time-now)
        ors-expl (update-one-run-state ors-hyps ep-explained {:compute 0 :memory 0})
        ors-meta (if (metareasoning-activated? ors-expl)
                   (metareason ors-expl) ors-expl)
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-next (proceed-one-run-state
                   ors-meta (current-ep-state (:ep-state-tree ors-meta)))
        ors-resources (assoc-in ors-next [:resources :milliseconds] ms)
        ors-results (evaluate truedata ors-resources)]
    (when (not player?)
      (.write System/out (int \.))) (.flush System/out)
    (if (and (not player?) monitor?)
      ((:monitor-fn @problem) truedata (:sensors ors-results) ors-results)
      ors-results)))

(defn run-simulation
  [truedata or-state monitor?]
  (loop [ors or-state]
    (dosync (alter retrospect.state/or-state (constantly ors)))
    (when (nil? ors)
      (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= (:time (:ep-state ors)) (:Steps params))
      (do (println "") (:results ors))
      (recur (run-simulation-step truedata ors monitor? false)))))

(defn get-default-params-ranges
  []
  (reduce (fn [m k] (assoc m k (second (get (:default-params @problem) k))))
          {} (keys (:default-params @problem))))

(defn get-default-params
  []
  (reduce (fn [m k] (assoc m k (first (get (:default-params @problem) k))))
          {} (keys (:default-params @problem))))

(defn merge-default-params
  [params]
  (let [default (get-default-params)]
    (merge default params)))

(defn run
  [comparative? monitor? params]
  (if comparative?
    ;; if comparative, run two simulations
    (let [[control-params comparison-params] (map merge-default-params params)
          control-results
          (binding [rgen (new-seed (:Seed control-params))
                    last-id 0
                    params control-params]
            (let [control-truedata ((:truedata-fn @problem))
                  control-sensors ((:sensor-gen-fn @problem))
                  control-problem-data ((:gen-problem-data-fn @problem)
                                        control-truedata control-sensors)
                  control-or-state (init-one-run-state control-sensors control-problem-data)]
              (println "Control:" (pr-str control-params))
              (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                   :comparison-params (pr-str comparison-params)))
                   (run-simulation control-truedata control-or-state monitor?))))
          comparison-results
          (binding [rgen (new-seed (:Seed comparison-params))
                    last-id 0
                    params comparison-params]
            (let [comparison-truedata ((:truedata-fn @problem))
                  comparison-sensors ((:sensor-gen-fn @problem))
                  comparison-problem-data ((:gen-problem-data-fn @problem)
                                           comparison-truedata comparison-sensors)
                  comparison-or-state (init-one-run-state comparison-sensors comparison-problem-data)]
              (println "Comparison:" (pr-str comparison-params))
              (map (fn [rs] (assoc rs
                              :control-params (pr-str control-params)
                              :comparison-params (pr-str comparison-params)))
                   (run-simulation comparison-truedata comparison-or-state monitor?))))]
      [control-results comparison-results
       (map (fn [rs] (assoc rs
                       :control-params (pr-str (first params))
                       :comparison-params (pr-str (second params))))
            (evaluate-comparative control-results comparison-results
                                  control-params comparison-params))])
    ;; if non-comparative, just run the simulation
    (binding [rgen (new-seed (:Seed params))
              last-id 0
              params params]
      (let [truedata ((:truedata-fn @problem))
            sensors ((:sensor-gen-fn @problem))
            problem-data ((:gen-problem-data-fn @problem) truedata sensors)
            or-state (init-one-run-state sensors problem-data)]
        (println "Params:" (pr-str params))
        (map (fn [rs] (assoc rs :params (pr-str params)))
             (run-simulation truedata or-state monitor?))))))

(defrecord Problem
  [name monitor-fn player-fns truedata-fn sensor-gen-fn prepared-map
   hypothesize-fn get-more-hyps-fn commit-decision-fn retract-fn
   gen-problem-data-fn inconsistent-fn no-explainer-hyps-fn
   evaluate-fn evaluate-comparative-fn true-hyp?-fn hyps-equal?-fn perturb-fn
   hyp-subtypes default-params])
