(ns retrospect.simulate
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep init-est update-est decision-points]])
  (:use [retrospect.sensors :only [update-sensors reset-sensors]])
  (:use [geppetto.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn run-simulation-step
  [truedata ors player?]
  (let [time-prev (:time (cur-ep (:est ors)))
        time-now (min (:Steps params) (+ (:StepsBetween params) time-prev))
        ;; start the clock
        start-time (. System (nanoTime))
        est-next (new-child-ep (:est ors))
        est-time (update-est est-next (assoc (cur-ep est-next) :time time-now))
        reason-est ((:reason-fn @reasoner) est-time time-prev time-now (:sensors ors))
        ;; record this ep as a decision point
        decision-est (update-est reason-est (assoc (cur-ep reason-est)
                                              :time time-now
                                              :decision-point true))
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-est (assoc ors :est decision-est)
        ors-results (update-in ors-est [:resources :milliseconds] + ms)]
    #_(retrospect.reason.abduction.workspace/view-accgraph (:workspace (cur-ep (:est ors-results))))
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
  ;; add to last set of results; make sure to grab the latest
  ;; decision ep so that the most recent results are included
  (let [new-results 
        (conj (:results (last (filter #(not= % (cur-ep (:est ors)))
                                 (decision-points (:est ors))))
                        [])
              (merge ((:evaluate-fn @reasoner) truedata (:est ors))
                     {:Milliseconds (get-in ors [:resources :milliseconds])}))
        est (update-est (:est ors)
                        (assoc (cur-ep (:est ors)) :results new-results))]
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
        ws-meta-oracle (if (= "oracle" (:Metareasoning params))
                         (assoc ws-oracle :meta-oracle
                                (partial (:meta-oracle-fn @reasoner) truedata))
                         ws-oracle)
        ws-with-kb ((:init-kb-fn @reasoner) ws-meta-oracle (:training truedata))
        est (new-child-ep (init-est ws-with-kb))]
    {:resources {:milliseconds 0 :meta-accepted 0 :meta-activations 0}
     :sensors sensors :est est}))

(def global-default-params
  {:Metareasoning ["none" ["none" "oracle"]]
   :UpdateKB [true [true false]]
   :Oracle ["none" ["none"]]
   :Steps [10 [10]]
   :Stats [false [false]]
   :ProbSensedLater [0 [0 20 40]]
   :Noise [false [true false]]
   :NoiseLevelsEqual [false [true false]]
   :NoiseLevels [0 [0 10]]
   :SensorDeletionNoise [0 [0 10]]
   :SensorInsertionNoise [0 [0 10]]
   :SensorDistortionNoise [0 [0 10]]
   :SensorDuplicationNoise [0 [0 10]]})

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
  (let [default (get-default-params)
        ps (merge default params)]
    (if (:NoiseLevelsEqual ps)
      (let [level (:NoiseLevels ps)]
        (assoc ps
          :SensorDeletionNoise level
          :SensorInsertionNoise level
          :SensorDistortionNoise level
          :SensorDuplicationNoise level))
      ps)))

(defn pre-sense
  [truedata sensors]
  (loop [t 0
         sens (if (:ResetSensors params) (reset-sensors sensors) sensors)]
    (let [sens2 (update-sensors sens (:test truedata) t)]
      (if (>= t (:Steps params)) sens2
          (recur (inc t) sens2)))))

(defn run-single
  [ps]
  (when (and (not @quiet-mode) (not (:Stats ps)))
    (prn ps))
  (binding [rgen (new-seed (:Seed ps))
            last-id 0
            cache (atom {})
            params ps]
    (let [truedata ((:generate-truedata-fn @problem))
          sensors (pre-sense truedata ((:generate-sensors-fn @problem) (:training truedata)))
          ors (init-ors truedata sensors)]
      (let [ors-final (run-simulation truedata ors)]
        (:results (cur-ep (:est ors-final)))))))

(defn run
  [comparative? params]
  (if comparative?
    ;; if comparative, run two simulations
    (let [[control-params comparison-params] (map merge-default-params params)
          control-results (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                            :comparison-params (pr-str comparison-params)))
                             (run-single control-params))
          comparison-results (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                               :comparison-params (pr-str comparison-params)))
                                (run-single comparison-params))]
      [control-results comparison-results
       (map (fn [rs] (assoc rs
                    :control-params (pr-str (first params))
                    :comparison-params (pr-str (second params))))
          ((:evaluate-comp-fn @reasoner) control-results comparison-results
           control-params comparison-params))])
    ;; if non-comparative, just run the simulation
    (let [control-params (merge-default-params params)]
      (map (fn [rs] (assoc rs :params (pr-str control-params)))
         (run-single control-params)))))
