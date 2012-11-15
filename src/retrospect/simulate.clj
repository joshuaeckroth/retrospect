(ns retrospect.simulate
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:require [clojure.set :as set])
  (:use [retrospect.profile :only [profile]])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep init-est update-est]])
  (:use [retrospect.sensors :only [update-sensors reset-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn update-sensors-from-to
  [time-prev time-now truedata sensors]
  (loop [t time-prev
         sens (if (:ResetSensors params) (reset-sensors sensors) sensors)]
    (let [sens2 (update-sensors sens (:test truedata) t)]
      (if (>= t time-now) sens2
          (recur (inc t) sens2)))))

(defn run-simulation-step
  [truedata ors player?]
  (let [time-prev (:time (cur-ep (:est ors)))
        time-now (min (:Steps params) (+ (:StepsBetween params) time-prev))
        sensors (update-sensors-from-to time-prev time-now truedata (:sensors ors))
        ;; start the clock
        start-time (. System (nanoTime))
        est-next (new-child-ep (:est ors))
        est-time (update-est est-next (assoc (cur-ep est-next) :time time-now))
        reason-est ((:reason-fn @reasoner) est-time time-prev time-now sensors)
        ;; record this ep as a decision point
        decision-est (update-est reason-est (assoc (cur-ep reason-est) :decision-point true))
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-est (assoc ors :est decision-est :sensors sensors)
        ors-results (update-in ors-est [:resources :milliseconds] + ms)]
    (when (:Stats params)
      ((:stats-fn @reasoner) truedata ors-results time-now))
    (comment
      (when (and (not player?) (not (:Stats params)))
        (.write System/out (int \.))
        (when (= 0 (mod time-now 1000)) (.print System/out (str time-now)))
        (.flush System/out)))
    ors-results))

(defn evaluate
  [truedata ors]
  (let [est (update-est (:est ors)
                        (update-in (cur-ep (:est ors)) [:results] conj
                                   (merge ((:evaluate-fn @reasoner) truedata (:est ors))
                                          {:Milliseconds (get-in ors [:resources :milliseconds])})))]
    (assoc ors :est est)))

(defn run-simulation
  [truedata or-state]
  (loop [ors or-state]
    (when (not @batch) (dosync (alter retrospect.state/or-state (constantly ors))))
    (if (>= (:time (cur-ep (:est ors))) (:Steps params))
      (evaluate truedata ors)
      (recur (run-simulation-step truedata ors false)))))

(defn init-ors
  [truedata sensors]
  (let [ws ((:init-workspace-fn @reasoner))
        ws-oracle (if (= "none" (:Oracle params)) ws
                      (assoc ws :oracle (partial (:oracle-fn @problem) truedata)))
        est (new-child-ep (init-est ((:init-kb-fn @reasoner) ws-oracle
                                     (:training truedata))))]
    {:resources {:milliseconds 0 :meta-accepted 0 :meta-activations 0}
     :sensors sensors :est est}))

(def global-default-params
  {:Metareasoning ["none" ["none"]]
   :UpdateKB [true [true false]]
   :Oracle ["none" ["none"]]
   :Steps [10 [10]]
   :Stats [false [false]]
   :SensorDeletionNoise [0 [0]]
   :SensorInsertionNoise [0 [0]]
   :SensorDistortionNoise [0 [0]]})

(defn get-default-params-ranges
  []
  (let [ps (merge global-default-params
                  (:default-params @problem)
                  ((:default-params-fn @reasoner)))]
    (reduce (fn [m k] (assoc m k (second (get ps k))))
            {} (keys ps))))

(defn get-default-params
  []
  (let [ps (merge global-default-params
                  (:default-params @problem)
                  ((:default-params-fn @reasoner)))]
    (reduce (fn [m k] (assoc m k (first (get ps k))))
            {} (keys ps))))

(defn merge-default-params
  [params]
  (let [default (get-default-params)]
    (merge default params)))

(defn run
  [comparative? params]
  (if comparative?
    ;; if comparative, run two simulations
    (let [[control-params comparison-params] (map merge-default-params params)
          control-results
          (binding [rgen (new-seed (:Seed control-params))
                    last-id 0
                    params control-params]
            (let [control-truedata (profile ((:generate-truedata-fn @problem)))
                  control-sensors ((:generate-sensors-fn @problem)
                                   (:training control-truedata))
                  control-ors (profile (init-ors control-truedata
                                                 control-sensors))]
              (comment
                (when (not (:Stats params))
                  (println "Control:" (pr-str control-params))))
              (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                :comparison-params (pr-str comparison-params)))
                 (let [ors-final (run-simulation control-truedata control-ors)]
                   (:results (cur-ep (:est ors-final)))))))
          comparison-results
          (binding [rgen (new-seed (:Seed comparison-params))
                    last-id 0
                    params comparison-params]
            (let [comparison-truedata (profile ((:generate-truedata-fn @problem)))
                  comparison-sensors ((:generate-sensors-fn @problem)
                                      (:training comparison-truedata))
                  comparison-ors (profile (init-ors comparison-truedata
                                                    comparison-sensors))]
              (comment
                (when (not (:Stats params))
                  (println "Comparison:" (pr-str comparison-params))))
              (map (fn [rs] (assoc rs
                           :control-params (pr-str control-params)
                           :comparison-params (pr-str comparison-params)))
                 (let [ors-final (run-simulation comparison-truedata comparison-ors)]
                   (:results (cur-ep (:est ors-final)))))))]
      [control-results comparison-results
       (map (fn [rs] (assoc rs
                    :control-params (pr-str (first params))
                    :comparison-params (pr-str (second params))))
          ((:evaluate-comp-fn @reasoner) control-results comparison-results
           control-params comparison-params))])
    ;; if non-comparative, just run the simulation
    (let [params (merge-default-params params)]
      (binding [rgen (new-seed (:Seed params))
                last-id 0
                params params]
        (let [truedata (profile ((:generate-truedata-fn @problem)))
              sensors ((:generate-sensors-fn @problem) (:training truedata))
              ors (profile (init-ors truedata sensors))]
          (comment (when (not (:Stats params))
                     (println "Params:" (pr-str params))))
          (map (fn [rs] (assoc rs :params (pr-str params)))
             (let [ors-final (run-simulation truedata ors)]
               (:results (cur-ep (:est ors-final))))))))))

