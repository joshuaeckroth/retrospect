(ns simulator.problem
  (:use [simulator.strategies :only (init-strat-states strategies run-simulation)]))

(def avg-fields
  [:Milliseconds :Steps
   :StrategyCompute :StrategyMilliseconds :StrategyMemory
   :SensorReportNoise :BeliefNoise])

(def non-avg-fields
  [:Strategy :MetaAbduce])

(defn start-player [problem] ((:player-fn problem)))

(defn run-strategies
  [problem params strats]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        strat-states (init-strat-states strats truedata sensors
                                        (:initial-problem-data problem))]
    (doall (for [ss strat-states]
             (run-simulation problem ss params)))))

(defn run-strategies-many
  [problem params n]
  (apply concat
         (for [i (range n)]
           (let [ss strategies]
             (run-strategies problem params ss)))))

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
    [name runner-fn player-fn truedata-fn sensor-gen-fn evaluate-fn
     initial-problem-data avg-fields non-avg-fields charts])
