(ns simulator.problem
  (:use [simulator.onerun :only [init-one-run-states run-simulation]]))

(def avg-fields
  [:Milliseconds :Steps
   :MetaAbductions :ExplainCompute :ExplainMilliseconds :ExplainMemory
   :SensorReportNoise :BeliefNoise])

(def non-avg-fields
  [:MetaAbduction])

(defn run-comparative
  [problem params]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        or-states (init-one-run-states {:MetaAbduction [true false]}
                                       sensors
                                       (:initial-problem-data problem))]
    (doall (for [ors or-states]
             ;; get last result set from each run
             (last (run-simulation problem truedata ors params))))))

(defn run-many
  [problem params n]
  (apply concat (for [i (range n)] (run-comparative problem params))))

(defn average-runs
  [problem params n]
  (let [results (run-many problem params n)]
    (doall (for [meta-abduction [true false]]
             (let [rs (filter #(= meta-abduction (:MetaAbduction %)) results)
            
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
    [name gen-hyps-fn player-fns truedata-fn sensor-gen-fn evaluate-fn
     initial-problem-data avg-fields non-avg-fields charts])
