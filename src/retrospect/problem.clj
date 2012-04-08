(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:use [retrospect.profile :only [profile]])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est
          update-est nth-previous-ep print-est goto-ep]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn meta-apply-and-evaluate
  [est new-est time-prev time-now sensors]
  (let [new-ep (cur-ep new-est)
        new-ws ((:reason-fn @reason) (:workspace new-ep) time-prev time-now sensors)
        new-expl-est (update-est new-est (assoc new-ep :workspace new-ws))]
    (if (> 0 ((:workspace-compare-fn @reason) new-ws (:workspace (cur-ep est))))
      new-expl-est
      (goto-ep new-expl-est (:id (cur-ep est))))))

(defn meta-batch
  [n est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (nth-previous-ep est n))
        new-est-time (update-est new-est (assoc (cur-ep new-est) :time time-now))]
    (meta-apply-and-evaluate est new-est-time time-prev time-now sensors)))

(defn meta-lower-threshold
  [est time-prev time-now sensors]
  (if (= 0 (:Threshold params)) est
      (let [new-est (new-branch-ep est (cur-ep est))]
        ;; drop threshold to 0
        (binding [params (assoc params :Threshold 0)]
          (meta-apply-and-evaluate est new-est time-prev time-now sensors)))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [est time-prev time-now sensors]
  (if (not ((:metareasoning-activated?-fn @reason) est)) est
      (let [m (:Metareasoning params)
            f (cond (= "BatchBeginning" m)
                    (partial meta-batch nil)
                    (= "Batch1" m)
                    (partial meta-batch 1)
                    (= "Batch2" m)
                    (partial meta-batch 2)
                    (= "Batch3" m)
                    (partial meta-batch 3)
                    (= "Batch4" m)
                    (partial meta-batch 4)
                    (= "Batch5" m)
                    (partial meta-batch 5)
                    (= "LowerThreshold" m)
                    meta-lower-threshold
                    :else (constantly est))]
        (f est time-prev time-now sensors))))

(defn init-ors
  [sensors training]
  (let [est (init-est ((:init-kb-fn @reason) ((:init-workspace-fn @reason)) training))]
    {:resources {:milliseconds 0 :meta-accepted 0 :meta-activations 0}
     :results []
     :sensors sensors
     :est est}))

(defn proceed-ors
  [ors est sensors time-now ms]
  (-> ors
      (assoc :est (new-child-ep est time-now))
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
  (profile
   (let [time-prev (:time (cur-ep (:est ors)))
         time-now (min (:Steps params) (+ (:StepsBetween params) time-prev))
         sensors (update-sensors-from-to time-prev time-now truedata (:sensors ors))
         ;; start the clock
         start-time (. System (nanoTime))
         ep (cur-ep (:est ors))
         workspace (if (and (not= 0 time-prev) (:ResetEachStep params))
                     (do (log "Resetting workspace...")
                         ((:init-workspace-fn @reason) (:workspace ep)))
                     (:workspace ep))
         ep-reason (assoc ep :workspace ((:reason-fn @reason) workspace
                                         time-prev time-now sensors))
         est (update-est (:est ors) ep-reason)
         meta-est (metareason est time-prev time-now sensors)
         ;; stop the clock
         ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
         ors-next (proceed-ors ors meta-est sensors time-now ms)
         ors-results ((:evaluate-fn @reason) truedata ors-next)]
     (when (not player?)
       (.write System/out (int \.)) (.flush System/out))
     ors-results)))

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

