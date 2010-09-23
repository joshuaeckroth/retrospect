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
        (filter #(= (type %) simulator.problems.circuit.hypotheses.BrokenGateHyp)
                (get (:accepted strat-state) time))
        correct-b-g-hyps
        (filter (fn [b-h] (some #(= (:gate-id b-h) %) broken-gates)) accepted-b-g-hyps)]
    {:TotalEvents (count broken-gates)
     :Correct (count correct-b-g-hyps)
     :Incorrect (- (count accepted-b-g-hyps) (count correct-b-g-hyps))
     :Observable observable-broken-gates
     :PercentCorrect
     (if (= 0 observable-broken-gates) 100.0
         (double (* 100 (/ (count correct-b-g-hyps) observable-broken-gates))))}))

(defn run
  [params strat-state]
  (let [[gates wiring] (rand-gates-wiring params)
        time 0
        input-vals (make-input-vals gates)
        startTime (. System (nanoTime))
        ss (assoc strat-state :problem-data
                  {:gates gates :wiring wiring :input-vals input-vals})
        ss2 (process gates wiring input-vals time params ss)
        evaluation (evaluate gates wiring time ss2)]
    {:stratstate ss2
     :results (merge params
                     (assoc evaluation
                       :Milliseconds (/ (double (- (. System (nanoTime)) startTime))
                                        1000000.0)
                       :Strategy (:strategy ss)
                       :StrategyCompute (:compute (:resources ss))
                       :StrategyMilliseconds (:milliseconds (:resources ss))
                       :StrategyMemory (:memory (:resources ss))))}))




