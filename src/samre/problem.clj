(ns samre.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [samre.onerun :only
         [init-one-run-states update-one-run-state proceed-one-run-state]])
  (:use [samre.epistemicstates :only
         [generate-hyps-and-explain previous-ep-state]])
  (:use [samre.meta.explain :only [explain-meta]])
  (:use [samre.sensors :only [update-sensors]]))

(def avg-fields
  [:Steps :Step :StepsBetween :MetaAbductions :Compute :Milliseconds :Memory
   :SensorReportNoise :BeliefNoise :Unexplained])

(def non-avg-fields
  [:MetaAbduction :Lazy])

(defn evaluate
  [problem truedata or-state params]
  (let [prev-ep (previous-ep-state (:ep-state-tree or-state))]
    (update-in or-state [:results] conj
               (merge ((:evaluate-fn problem)
                       (:ep-state or-state)
                       (:sensors or-state) truedata params)
                      (assoc params
                        :Step (:time (:ep-state or-state))
                        :MetaAbduction (:meta-abduction or-state)
                        :Lazy (:lazy or-state)
                        :MetaAbductions (:meta-abductions (:resources or-state))
                        :Compute (:compute (:resources or-state))
                        :Milliseconds (:milliseconds (:resources or-state))
                        :Memory (:memory (:resources or-state))
                        :Unexplained
                        (if prev-ep (count (:unexplained (:workspace prev-ep))) 0))))))

(defn do-process
  [problem or-state]
  (update-one-run-state or-state
   ((:process-fn problem) (:ep-state or-state) (:sensors or-state))))

(defn do-commit
  [problem or-state time params]
  (proceed-one-run-state or-state ((:commit-fn problem) (:ep-state or-state) time)
                         problem params))

(defn run-simulation-step
  [problem truedata or-state time params monitor player]
  (let [commit (= 0 (mod time (:CommitDelay params)))
        ors-sensors (update-in or-state [:sensors] update-sensors (get truedata time) time)
        ors-processed (do-process problem ors-sensors)
        start-time (. System (nanoTime))
        ors-committed (if-not commit ors-processed (do-commit problem ors-processed time))
        milliseconds (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-resources (update-in ors-committed [:resources]
                                 assoc :milliseconds milliseconds)
        ors-results (evaluate problem truedata ors-resources params)]
    (if (and (not player) monitor)
      ((:monitor-fn problem) problem truedata (:sensors ors-results) ors-results params)
      ors-results)))

(defn run-simulation
  [problem truedata or-state monitor params]
  (loop [time 0
         ors or-state]
    (when (nil? ors) (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= time (:Steps params)) (:results ors)
        (recur (inc time)
               (run-simulation-step problem truedata ors time params monitor false)))))

(defn run-comparative
  [problem monitor params]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        problem-data ((:gen-problem-data-fn problem) sensors params)
        or-states (init-one-run-states {:MetaAbduction [false] :Lazy [true]}
                                       sensors problem-data)]
    (doall (for [ors or-states]
             (last (run-simulation problem monitor truedata ors params))))))

(defn run-many
  [problem monitor params n]
  (apply concat (for [i (range n)] (run-comparative problem monitor params))))

(defn average-runs
  [problem monitor params n]
  (let [results (run-many problem monitor params n)]
    (doall (for [meta-abduction [false] lazy [true]]
             (let [rs (filter #(and (= meta-abduction (:MetaAbduction %))
                                    (= lazy (:Lazy %)))
                              results)
            
                   ;; choose any result; 'avg-fields' will be updated
                   result (first rs) 
        
                   avg (fn [field] (double (/ (reduce + (map field rs)) n)))
                   newfields (interleave (concat avg-fields (:avg-fields problem))
                                         (doall (map #(avg %)
                                                     (concat avg-fields
                                                             (:avg-fields problem)))))]
               (apply assoc result newfields))))))

(defn get-headers
  [problem]
  (concat avg-fields non-avg-fields (:avg-fields problem) (:non-avg-fields problem)))

(defrecord Problem
    [name monitor-fn player-fns truedata-fn sensor-gen-fn
     get-more-hyps-fn gen-problem-data-fn process-fn commit-fn accept-decision-fn
     evaluate-fn avg-fields non-avg-fields charts])
