(ns retrospect.simulate
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:require [clojure.set :as set])
  (:use [retrospect.profile :only [profile]])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est ep-state-depth
          update-est nth-previous-ep print-est goto-ep
          get-init-workspace]])
  (:use [retrospect.sensors :only [update-sensors reset-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn evaluate
  [truedata est meta-considered? meta-accepted-branch?]
  ;; TODO: grab MetaConsidered/MetaAccepted from prior results, not current results
  (let [results ((:evaluate-fn @reasoner) truedata est)
        prior-considered (get (last (:results (cur-ep est))) :MetaConsidered 0)
        prior-accepted (get (last (:results (cur-ep est))) :MetaAccepted 0)
        meta-results (-> results
                        (assoc-in [:MetaConsidered]
                                  (if meta-considered?
                                    (inc prior-considered) prior-considered))
                        (assoc-in [:MetaAccepted]
                                  (if meta-accepted-branch?
                                    (inc prior-accepted) prior-accepted)))]
    (update-est est (update-in (cur-ep est) [:results] conj meta-results))))

(defn meta-apply-and-evaluate
  [truedata est new-est time-prev time-now sensors]
  (let [new-ep (cur-ep new-est)
        ws-old (:workspace new-ep)
        ws-new ((:reason-fn @reasoner)
                (when (:Oracle params) truedata)
                (if (or (nil? sensors) (not (:ResetEachStep params))) ws-old
                    ((:reset-workspace-fn @reasoner) ws-old))
                time-prev time-now sensors)
        new-expl-est (update-est new-est (assoc new-ep :workspace ws-new))]
    (if ((:workspace-better?-fn @reasoner) ws-new ws-old)
      {:est new-expl-est
       :considered? true
       :accepted-branch? true}
      {:est (goto-ep new-expl-est (:id (cur-ep est)))
       :considered? true
       :accepted-branch? false})))

(defn meta-batch
  [n truedata est _ time-now sensors]
  (let [branch-root? (or (nil? n) (>= n (ep-state-depth est)))
        branch-ep (nth-previous-ep est n)
        new-est (new-branch-ep est branch-ep)
        new-est-time (update-est new-est
                                 (assoc (cur-ep new-est) :time time-now
                                        :workspace (if branch-root?
                                                     (get-init-workspace est)
                                                     (:workspace (cur-ep new-est)))))]
    (meta-apply-and-evaluate truedata est new-est-time
                             (if branch-root? 0 (:time branch-ep))
                             time-now sensors)))

(defn meta-lower-threshold
  [truedata est time-prev time-now sensors]
  (if (= 0 (:Threshold params))
    {:est est :considered? false :accepted-branch? false}
    (let [new-est (new-branch-ep est (cur-ep est))]
      ;; drop threshold to 0
      (binding [params (assoc params :Threshold 0)]
        ;; give sensors value as nil to prevent resensing
        (meta-apply-and-evaluate truedata est new-est time-prev time-now nil)))))

(defn meta-batch-lower-threshold
  [n truedata est time-prev time-now sensors]
  (let [batch-result (meta-batch n truedata est time-prev time-now sensors)]
    (if (or (= 0 (:Threshold params)) (:accepted-branch? batch-result))
      batch-result
      ;; how much to lower threshold?
      (binding [params (assoc params :Threshold 0)]
        (meta-batch n truedata est time-prev time-now sensors)))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [truedata est time-prev time-now sensors]
  (if (not ((:metareasoning-activated?-fn @reasoner) est))
    {:est est :considered? false :accepted-branch? false}
    (let [m (:Metareasoning params)
          f (cond (= "batchbeg" m)
                  (partial meta-batch nil)
                  (= "batch1" m)
                  (partial meta-batch 1)
                  (= "batch2" m)
                  (partial meta-batch 2)
                  (= "batch3" m)
                  (partial meta-batch 3)
                  (= "batch4" m)
                  (partial meta-batch 4)
                  (= "batch5" m)
                  (partial meta-batch 5)
                  (= "lowerthresh" m)
                  meta-lower-threshold
                  (= "batchlower1" m)
                  (partial meta-batch-lower-threshold 1)
                  ;; did not recognize the metareasoning strategy;
                  ;; hand-off to the reasoning engine
                  :else
                  (:metareason-fn @reasoner))]
      (f truedata est time-prev time-now sensors))))

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
        ors-new (if-not (:GrowEst params)
                  (update-in ors [:est] update-est
                             (assoc (cur-ep (:est ors)) :time time-now))
                  (update-in ors [:est] new-child-ep time-now))
        ep (cur-ep (:est ors-new))
        workspace (if (and (not= 0 time-prev) (:ResetEachStep params))
                    (do (log "Resetting workspace...")
                        ((:reset-workspace-fn @reasoner) (:workspace ep)))
                    (:workspace ep))
        workspace-reasoned ((:reason-fn @reasoner)
                            (when (:Oracle params) truedata)
                            workspace time-prev time-now sensors)
        ep-reason (assoc ep :workspace workspace-reasoned)
        {meta-est :est considered? :considered? accepted-branch? :accepted-branch?}
        (metareason truedata (update-est (:est ors-new) ep-reason)
                    time-prev time-now sensors)
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        meta-est-eval (evaluate truedata meta-est considered? accepted-branch?)
        ors-est (assoc ors-new :est meta-est-eval :sensors sensors)
        ors-results (update-in ors-est [:resources :milliseconds] + ms)]
    (when (:Stats params)
      ((:stats-fn @reasoner) truedata ors-results time-now))
    (when (and (not player?) (not (:Stats params)))
      (.write System/out (int \.))
      (when (= 0 (mod time-now 1000)) (.print System/out (str time-now)))
      (.flush System/out))
    ors-results))

(defn run-simulation
  [truedata or-state]
  (loop [ors or-state]
    (when (not @batch) (dosync (alter retrospect.state/or-state (constantly ors))))
    (if (>= (:time (cur-ep (:est ors))) (:Steps params))
      (do (println "") ors)
      (recur (run-simulation-step truedata ors false)))))

(defn init-ors
  [sensors training]
  (let [est (init-est ((:init-kb-fn @reasoner) ((:init-workspace-fn @reasoner)) training))]
    {:resources {:milliseconds 0 :meta-accepted 0 :meta-activations 0}
     :sensors sensors :est est}))

(def global-default-params
  {:Metareasoning ["none" ["none"]]
   :Knowledge [80 [80]]
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
                  control-sensors ((:generate-sensors-fn @problem) control-truedata)
                  control-ors (profile (init-ors control-sensors
                                                 (:training control-truedata)))]
              (when (not (:Stats params))
                (println "Control:" (pr-str control-params)))
              (map (fn [rs] (assoc rs :control-params (pr-str control-params)
                                   :comparison-params (pr-str comparison-params)))
                   (let [ors-final (run-simulation control-truedata control-ors)]
                     (:results (cur-ep (:est ors-final)))))))
          comparison-results
          (binding [rgen (new-seed (:Seed comparison-params))
                    last-id 0
                    params comparison-params]
            (let [comparison-truedata (profile ((:generate-truedata-fn @problem)))
                  comparison-sensors ((:generate-sensors-fn @problem) comparison-truedata)
                  comparison-ors (profile (init-ors comparison-sensors
                                                    (:training comparison-truedata)))]
              (when (not (:Stats params))
                (println "Comparison:" (pr-str comparison-params)))
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
              sensors ((:generate-sensors-fn @problem) truedata)
              ors (profile (init-ors sensors (:training truedata)))]
          (when (not (:Stats params))
            (println "Params:" (pr-str params)))
          (map (fn [rs] (assoc rs :params (pr-str params)))
               (let [ors-final (run-simulation truedata ors)]
                 (:results (cur-ep (:est ors-final))))))))))

