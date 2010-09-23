(ns simulator.problems.circuit.core
  (:use [simulator.strategies :only (init-strat-state explain)])
  (:use [simulator.problems.circuit.circuit])
  (:use [simulator.problems.circuit.hypotheses]))

(defn process
  [gates wiring input-vals time params strat-state]
  (let [ss-with-hyps (generate-hyps strat-state time gates wiring input-vals params)
        ss-explained (explain ss-with-hyps time)]
    ss-explained))

(defn evaluate
  [gates wiring time strat-state]
  (let [broken-gates (find-broken-gates gates)
        observable-broken-gates
        (- (count broken-gates) (count (find-undetectable-broken-gates gates wiring)))
        accepted-b-g-hyps
        (filter #(= (type %) simulator.problems.circuit.core.BrokenGateHyp)
                (get (:accepted strat-state) time))
        correct-b-g-hyps
        (filter (fn [b-h] (some #(= (:gate-id b-h) %) broken-gates)) accepted-b-g-hyps)]
    {:TotalEvents (count broken-gates)
     :Correct (count correct-b-g-hyps)
     :Incorrect (- (count accepted-b-g-hyps) (count correct-b-g-hyps))
     :Observable observable-broken-gates
     :PercentCorrect
     (if (empty? broken-gates) 100.0
         (double (* 100 (/ (count correct-b-g-hyps) (count broken-gates)))))}))

(defn run
  [params strat-state]
  (let [[gates wiring] (rand-gates-wiring params)
        time 0
        input-vals (make-input-vals gates)
        startTime (. System (nanoTime))
        ss (process gates wiring input-vals time params strat-state)
        evaluation (evaluate gates wiring time ss)]
    {:stratstate ss
     :results (merge params
                     (assoc evaluation
                       :Milliseconds (/ (double (- (. System (nanoTime)) startTime))
                                        1000000.0)
                       :Strategy (:strategy ss)
                       :StrategyCompute (:compute (:resources ss))
                       :StrategyMilliseconds (:milliseconds (:resources ss))
                       :StrategyMemory (:memory (:resources ss))))}))




