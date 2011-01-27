(ns samre.problem
  (:use [samre.onerun :only
         [init-one-run-states update-one-run-state proceed-one-run-state]])
  (:use [samre.epistemicstates :only
         [generate-hyps-and-explain previous-ep-state]])
  (:use [samre.meta.explain :only [explain-meta]])
  (:use [samre.sensors :only [update-sensors]]))

(def avg-fields
  [:Steps :StepsBetween :MetaAbductions :Compute :Milliseconds :Memory
   :SensorReportNoise :BeliefNoise])

(def non-avg-fields
  [:MetaAbduction :Lazy])

(defn evaluate
  [problem truedata or-state params]
  (update-in or-state [:results] conj
             (merge ((:evaluate-fn problem)
                     (:ep-state or-state)
                     (:sensors or-state) truedata params)
                    (assoc params
                      :MetaAbduction (:meta-abduction or-state)
                      :Lazy (:lazy or-state)
                      :MetaAbductions (:meta-abductions (:resources or-state))
                      :Compute (:compute (:resources or-state))
                      :Milliseconds (:milliseconds (:resources or-state))
                      :Memory (:memory (:resources or-state))))))

(defn proceed-n-steps
  [n time truedata or-state]
  (loop [t time
         ors or-state]
    (if (= t (+ n time)) ors
        (recur (inc t)
               (update-in ors [:sensors] update-sensors (get truedata t) t)))))

(defn run-simulation-step
  [problem truedata or-state params]
  (let [ors (proceed-n-steps (:StepsBetween params) (:time (:ep-state or-state))
                             truedata or-state)
        time-now (+ (dec (:StepsBetween params)) (:time (:ep-state ors)))
        time-prev (if-let [time-prev (:time (previous-ep-state (:ep-state-tree ors)))]
                    (inc time-prev) 0)
        start-time (. System (nanoTime))
        ep-state (generate-hyps-and-explain problem (:ep-state ors) time-prev time-now
                                            (:sensors ors) params (:lazy ors))
        ors-explained (update-one-run-state ors ep-state)
        ors-meta (explain-meta problem ors-explained params)
        ep-state-meta (:ep-state ors-meta)
        ors-next (proceed-one-run-state ors-meta ep-state-meta problem params)
        milliseconds (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-final (update-in ors-next [:resources] assoc :milliseconds milliseconds)]
    (evaluate problem truedata ors-final params)))

(defn run-simulation
  [problem truedata or-state params]
  (loop [ors or-state]
    (if (>= (:time (:ep-state ors)) (- (:Steps params) (:StepsBetween params))) (:results ors)
        (recur (run-simulation-step problem truedata ors params)))))

(defn run-comparative
  [problem params]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        problem-data ((:gen-problem-data-fn problem) params sensors)
        or-states (init-one-run-states {:MetaAbduction [true false] :Lazy [false]}
                                       sensors problem-data)]
    (doall (for [ors or-states]
             ;; get last result set from each run
             (last (run-simulation problem truedata ors params))))))

(defn run-many
  [problem params n]
  (apply concat (for [i (range n)] (run-comparative problem params))))

(defn average-runs
  [problem params n]
  (let [results (run-many problem params n)]
    (doall (for [meta-abduction [true false] lazy [false]]
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

(defn get-headers [problem] (concat avg-fields non-avg-fields
                                    (:avg-fields problem) (:non-avg-fields problem)))

(defrecord Problem
    [name get-more-hyps-fn player-fns truedata-fn sensor-gen-fn
     evaluate-fn gen-problem-data-fn accept-decision-fn avg-fields non-avg-fields charts])
