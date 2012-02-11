(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:use [retrospect.epistemicstates :only [cur-ep new-child-ep init-est update-est]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.state]))

(defn init-ors
  [sensors]
  (let [est (init-est ((:init-workspace-fn @reason)))]
    {:resources {:milliseconds 0}
     :results []
     :sensors sensors
     :est est}))

(defn proceed-ors
  [ors ep sensors ms]
  (-> ors
      (update-in [:est] #(new-child-ep (update-est % ep)))
      (assoc :sensors sensors)
      (update-in [:resources :milliseconds] + ms)))

(defn update-sensors-from-to
  [time time-now truedata sensors]
  (loop [t time
         sens sensors]
    (let [sens2 (update-sensors sens truedata t)]
      (if (>= t time-now) sens2
          (recur (inc t) sens2)))))

(defn run-simulation-step
  [truedata ors monitor? player?]
  (let [time (or (:time (cur-ep (:est ors))) 0)
        time-now (min (:Steps params) (+ (:StepsBetween params) time))
        sensors (update-sensors-from-to time time-now truedata (:sensors ors))
        ;; start the clock
        start-time (. System (nanoTime))
        ep (cur-ep (:est ors))
        ep-reason (update-in ep [:workspace] (:reason-fn @reason) time time-now sensors)
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-next (proceed-ors ors ep-reason sensors ms)
        ors-results ((:evaluate-fn @reason) truedata ors-next)]
    (when (not player?)
      (.write System/out (int \.))) (.flush System/out)
    (if (and (not player?) monitor?)
      ((:monitor-fn @problem) truedata sensors ors-results)
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
  (let [ps (merge (:default-params @problem) ((:default-params-fn @reason)))]
    (reduce (fn [m k] (assoc m k (second (get ps k))))
            {} (keys ps))))

(defn get-default-params
  []
  (let [ps (merge (:default-params @problem) ((:default-params-fn @reason)))]
    (reduce (fn [m k] (assoc m k (first (get ps k))))
            {} (keys ps))))

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
            (let [control-truedata ((:generate-truedata-fn @problem))
                  control-sensors ((:generate-sensors-fn @problem))
                  control-ors (init-ors control-sensors)]
              (println "Control:" (pr-str control-params))
              (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                   :comparison-params (pr-str comparison-params)))
                   (run-simulation control-truedata control-ors monitor?))))
          comparison-results
          (binding [rgen (new-seed (:Seed comparison-params))
                    last-id 0
                    params comparison-params]
            (let [comparison-truedata ((:generate-truedata-fn @problem))
                  comparison-sensors ((:generate-sensors-fn @problem))
                  comparison-ors (init-ors comparison-sensors)]
              (println "Comparison:" (pr-str comparison-params))
              (map (fn [rs] (assoc rs
                              :control-params (pr-str control-params)
                              :comparison-params (pr-str comparison-params)))
                   (run-simulation comparison-truedata comparison-ors monitor?))))]
      [control-results comparison-results
       (map (fn [rs] (assoc rs
                       :control-params (pr-str (first params))
                       :comparison-params (pr-str (second params))))
            ((:evaluate-comp-fn @reason) control-results comparison-results
             control-params comparison-params))])
    ;; if non-comparative, just run the simulation
    (let [params (merge-default-params params)]
      (binding [rgen (new-seed (:Seed params))
                last-id 0
                params params]
        (let [truedata ((:generate-truedata-fn @problem))
              sensors ((:generate-sensors-fn @problem))
              ors (init-ors sensors)]
          (println "Params:" (pr-str params))
          (map (fn [rs] (assoc rs :params (pr-str params)))
               (run-simulation truedata ors monitor?)))))))

