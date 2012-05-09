(ns retrospect.simulate
  (:import (java.util.concurrent ExecutionException))
  (:use [clojure.string :only [split]])
  (:require [clojure.set :as set])
  (:use [retrospect.profile :only [profile]])
  (:use [retrospect.reason.abduction.workspace :only [add accept find-conflicts]])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est ep-state-depth
          update-est nth-previous-ep print-est goto-ep
          get-init-workspace]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [rgen new-seed my-rand-int]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn evaluate
  [truedata est]
  (update-est est (update-in (cur-ep est) [:results] conj
                             ((:evaluate-fn @reason) truedata est))))

(defn meta-apply-and-evaluate
  [truedata est new-est time-prev time-now sensors]
  (let [new-ep (cur-ep new-est)
        new-ws2 ((:reason-fn @reason) (when (:Oracle params) truedata)
                 ((:reset-workspace-fn @reason) (:workspace new-ep)) time-prev time-now sensors)
        new-ws (reduce (fn [ws hyp]
                         (let [conflicts (find-conflicts ws hyp)]
                           (-> (reduce (fn [w h] (assoc-in w [:accepted (:type h)]
                                                           (filter #(not= h %)
                                                                   (get-in w [:accepted (:type h)]))))
                                       ws conflicts)
                               (add hyp)
                               (accept hyp [] [] nil false))))
                       (:workspace (cur-ep est))
                       (set/difference (set (apply concat (vals (:accepted new-ws2))))
                                       (set (apply concat (vals (:accepted (:workspace (cur-ep est))))))))
        new-expl-est (update-est new-est (assoc new-ep :workspace new-ws))]
    (println (set/difference (set (apply concat (vals (:accepted new-ws2))))
                             (set (apply concat (vals (:accepted (:workspace (cur-ep est))))))))
    (println "after learning")
    ((:evaluate-fn @reason) truedata (update-est new-est (assoc new-ep :workspace new-ws2)))
    (if (> 0 ((:workspace-compare-fn @reason) new-ws2 (:workspace (cur-ep est))))
      new-expl-est
      (goto-ep new-expl-est (:id (cur-ep est))))))

(defn meta-batch
  [n truedata est _ time-now sensors]
  (let [branch-root? (or (nil? n) (>= (ep-state-depth est) n))
        branch-ep (nth-previous-ep est n)
        new-est (new-branch-ep est branch-ep)
        new-est-time (update-est new-est (assoc (cur-ep new-est) :time time-now
                                                :workspace (if branch-root?
                                                             (get-init-workspace est)
                                                             (:workspace (cur-ep new-est)))))]
    (meta-apply-and-evaluate truedata est new-est-time
                             (if branch-root? 0 (:time branch-ep))
                             time-now sensors)))

(defn meta-lower-threshold
  [truedata est time-prev time-now sensors]
  (if (= 0 (:Threshold params)) est
      (let [new-est (new-branch-ep est (cur-ep est))]
        ;; drop threshold to 0
        (binding [params (assoc params :Threshold 0)]
          ;; give sensors value as nil to prevent resensing
          (meta-apply-and-evaluate truedata est new-est time-prev time-now nil)))))

(defn meta-learn
  [truedata est time-prev time-now sensors]
  (println "\n\nbefore learning")
  ((:evaluate-fn @reason) truedata est)
  (let [new-est (new-branch-ep est (cur-ep est))]
    ;; activate learning
    (binding [params (assoc params :HypTypes "merge-noexp,words" :Threshold 30)]
      (meta-apply-and-evaluate truedata est new-est time-prev time-now nil))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [truedata est time-prev time-now sensors]
  (if (not ((:metareasoning-activated?-fn @reason) est)) est
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
                    (= "learn" m)
                    meta-learn
                    :else (constantly est))]
        (f truedata est time-prev time-now sensors))))

(defn init-ors
  [sensors training]
  (let [est (init-est ((:init-kb-fn @reason) ((:init-workspace-fn @reason)) training))]
    {:resources {:milliseconds 0 :meta-accepted 0 :meta-activations 0}
     :sensors sensors
     :est est}))

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
         ors-new (update-in ors [:est] new-child-ep time-now)
         ep (cur-ep (:est ors-new))
         workspace (if (and (not= 0 time-prev) (:ResetEachStep params))
                     (do (log "Resetting workspace...")
                         ((:init-workspace-fn @reason) (:workspace ep)))
                     (:workspace ep))
         ep-reason (assoc ep :workspace ((:reason-fn @reason) (when (:Oracle params) truedata)
                                         workspace time-prev time-now sensors))
         meta-est (metareason truedata (update-est (:est ors-new) ep-reason)
                              time-prev time-now sensors)
         ;; stop the clock
         ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
         meta-est-eval (evaluate truedata meta-est)
         ors-est (assoc ors-new :est meta-est-eval :sensors sensors)
         ors-results (update-in ors-est [:resources :milliseconds] + ms)]
     (when (:Stats params)
       ((:stats-fn @reason) truedata ors-results time-now))
     (when (not player?)
       (.write System/out (int \.)) (.flush System/out))
     ors-results)))

(defn run-simulation
  [truedata or-state]
  (loop [ors or-state]
    (dosync (alter retrospect.state/or-state (constantly ors)))
    (if (>= (:time (cur-ep (:est ors))) (:Steps params))
      (do (println "") (:results (cur-ep (:est ors))))
      (recur (run-simulation-step truedata ors false)))))

(def global-default-params
  {:Metareasoning ["none" ["none" "learn"]]
   :Oracle ["none" ["none"]]
   :Stats [false [false]]})

(defn get-default-params-ranges
  []
  (let [ps (merge global-default-params
                  (:default-params @problem)
                  ((:default-params-fn @reason)))]
    (reduce (fn [m k] (assoc m k (second (get ps k))))
            {} (keys ps))))

(defn get-default-params
  []
  (let [ps (merge global-default-params
                  (:default-params @problem)
                  ((:default-params-fn @reason)))]
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
                  control-ors (init-ors control-sensors (:training control-truedata))]
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
                  comparison-ors (init-ors comparison-sensors (:training comparison-truedata))]
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

