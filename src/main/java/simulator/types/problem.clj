(ns simulator.types.problem
  (:use [simulator.strategies :only (init-strat-states strategies)]))

(defn start-player [problem] ((:player-fn problem)))

(defn average-strategy-runs
  [problem params n]
  (let [strats (take 3 (shuffle strategies))
        runs (doall (apply concat
                           (doall (for [i (range n)]
                                    ((:runner-fn problem) params
                                     (init-strat-states strats
                                                        (:problem-data problem)))))))]
    (for [s strats]
      (let [rs (filter #(= s (:Strategy %)) runs)
            
            ;; choose any result; 'avg-fields' will be updated
            result (first rs) 
        
            avg (fn [field] (double (/ (reduce + (map field rs)) n)))
            newfields (interleave (:avg-fields problem)
                                  (map #(avg %) (:avg-fields problem)))]
        (apply assoc result newfields)))))

(defn get-headers [problem] (:headers problem))

(defrecord Problem
  [name runner-fn player-fn headers avg-fields non-avg-fields charts problem-data])
