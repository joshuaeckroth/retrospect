(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:use [retrospect.epistemicstates :only [cur-ep new-child-ep init-est update-est]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.state]))

(defn init-ors
  [sensors training]
  (let [est (init-est ((:init-workspace-fn @reason) training))]
    {:resources {:milliseconds 0}
     :results []
     :sensors sensors
     :est est}))

(defn proceed-ors
  [ors ep sensors time-now ms]
  (-> ors
      (update-in [:est] new-child-ep ep time-now)
      (assoc :sensors sensors)
      (update-in [:resources :milliseconds] + ms)))

(defn update-sensors-from-to
  [time-prev time-now truedata sensors]
  (loop [t time-prev
         sens sensors]
    (let [sens2 (update-sensors sens (:test truedata) t)]
      (if (>= t time-now) sens2
          (recur (inc t) sens2)))))

(defn run-simulation-step
  [truedata ors player?]
  (let [time-prev (or (:time (cur-ep (:est ors))) 0)
        time-now (min (:Steps params) (+ (:StepsBetween params) time-prev))
        sensors (update-sensors-from-to time-prev time-now truedata (:sensors ors))
        ;; start the clock
        start-time (. System (nanoTime))
        ep (cur-ep (:est ors))
        workspace (if (and (not= 0 time-prev) (:ResetEachStep params))
                    ((:init-workspace-fn @reason) (:training truedata))
                    (:workspace ep))
        ep-reason (assoc ep :workspace ((:reason-fn @reason) workspace
                                        time-prev time-now sensors))
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-next (proceed-ors ors ep-reason sensors time-now ms)
        ors-results ((:evaluate-fn @reason) truedata ors-next)]
    (when (not player?)
      (.write System/out (int \.)) (.flush System/out))
    ors-results))

(defn run-simulation
  [truedata or-state]
  (loop [ors or-state]
    (dosync (alter retrospect.state/or-state (constantly ors)))
    (if (>= (:time (cur-ep (:est ors))) (:Steps params))
      (do (println "") (:results ors))
      (recur (run-simulation-step truedata ors false)))))

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
  [comparative? params]
  (if comparative?
    ;; if comparative, run two simulations
    (let [[control-params comparison-params] (map merge-default-params params)
          control-results
          (binding [rgen (new-seed (:Seed control-params))
                    last-id 0
                    params control-params]
            (let [control-truedata ((:generate-truedata-fn @problem))
                  control-sensors ((:generate-sensors-fn @problem))
                  control-ors (init-ors control-sensors (:training truedata))]
              (println "Control:" (pr-str control-params))
              (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                   :comparison-params (pr-str comparison-params)))
                   (run-simulation control-truedata control-ors))))
          comparison-results
          (binding [rgen (new-seed (:Seed comparison-params))
                    last-id 0
                    params comparison-params]
            (let [comparison-truedata ((:generate-truedata-fn @problem))
                  comparison-sensors ((:generate-sensors-fn @problem))
                  comparison-ors (init-ors comparison-sensors (:training truedata))]
              (println "Comparison:" (pr-str comparison-params))
              (map (fn [rs] (assoc rs
                              :control-params (pr-str control-params)
                              :comparison-params (pr-str comparison-params)))
                   (run-simulation comparison-truedata comparison-ors))))]
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
              ors (init-ors sensors (:training truedata))]
          (println "Params:" (pr-str params))
          (map (fn [rs] (assoc rs :params (pr-str params)))
               (run-simulation truedata ors)))))))

