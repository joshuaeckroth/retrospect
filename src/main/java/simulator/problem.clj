(ns simulator.problem
  (:use [simulator.onerun :only [init-one-run-states run-simulation]])
  (:use [simulator.strategies.composite :only [strategies]])
  (:use [simulator.strategies.metastrategies :only [meta-strategies]]))

(def avg-fields
  [:Milliseconds :Steps
   :StrategyCompute :StrategyMilliseconds :StrategyMemory
   :SensorReportNoise :BeliefNoise])

(def non-avg-fields
  [:Strategy :MetaStrategy])

(defn run-strategies
  [problem params]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        or-states (init-one-run-states strategies meta-strategies sensors
                                       (:initial-problem-data problem))]
    (doall (for [ors or-states]
             ;; get last result set from each run
             (last (run-simulation problem truedata ors params))))))

(defn run-strategies-many
  [problem params n]
  (apply concat (for [i (range n)] (run-strategies problem params))))

(defn average-strategies
  [problem params n]
  (let [results (run-strategies-many problem params n)]
    (doall (for [s strategies ms meta-strategies]
             (let [rs (filter #(and (= ms (:MetaStrategy %))
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
