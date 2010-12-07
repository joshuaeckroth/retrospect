(ns simulator.problem
  (:use [simulator.strategies :only (init-one-run-states strategies run-simulation)]))

(def avg-fields
  [:Milliseconds :Steps
   :StrategyCompute :StrategyMilliseconds :StrategyMemory
   :SensorReportNoise :BeliefNoise])

(def non-avg-fields
  [:Strategy :MetaAbduce])

(defn run-strategies
  [problem params strats]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        or-states (init-one-run-states strats sensors (:initial-problem-data problem))]
    (doall (for [ors or-states]
             ;; get last result set
             (last (run-simulation problem truedata ors params))))))

(defn run-strategies-many
  [problem params n]
  (apply concat
         (for [i (range n)]
           (let [s strategies]
             (run-strategies problem params s)))))

(defn average-strategies
  [problem params n]
  (let [results (run-strategies-many problem params n)]
    (doall (for [s strategies meta-abduce ["true" "false"]]
             (let [rs (filter #(and (= meta-abduce (:MetaAbduce %))
                                    (= s (:Strategy %))) results)
            
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
    [name gen-hyps-fn update-problem-data-fn
     player-fns truedata-fn sensor-gen-fn evaluate-fn
     initial-problem-data avg-fields non-avg-fields charts])
