(ns samre.problems.circuit.core
  (:use [samre.strategies :only (explain)])
  (:use [samre.problems.circuit.circuit])
  (:use [samre.problems.circuit.hypotheses]))

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
        (filter #(= (type %) samre.problems.circuit.hypotheses.BrokenGateHyp)
                (get (:accepted strat-state) time))
        correct-b-g-hyps
        (filter (fn [b-h] (some #(= (:gate-id b-h) %) broken-gates)) accepted-b-g-hyps)]
    {:Gates (count gates)
     :TotalBroken (count broken-gates)
     :Correct (count correct-b-g-hyps)
     :Incorrect (- (count accepted-b-g-hyps) (count correct-b-g-hyps))
     :Observable observable-broken-gates
     :ObservablePercent
     (if (empty? broken-gates) 100.0
         (double (* 100 (/ observable-broken-gates (count broken-gates)))))
     :PercentCorrect
     (if (= 0 observable-broken-gates) 100.0
         (double (* 100 (/ (count correct-b-g-hyps) observable-broken-gates))))}))

(defn run
  [params strat-states]
  (let [[gates wiring] (rand-gates-wiring params)
        time 0
        input-vals (make-input-vals gates)
        startTime (. System (nanoTime))
        sss (map #(assoc % :problem-data
                         {:gates gates :wiring wiring :input-vals input-vals})
                 strat-states)
        sss2 (map #(process gates wiring input-vals time params %) sss)]
    (for [ss sss2]
      {:stratstate ss
       :results (merge params
                       (assoc (evaluate gates wiring time ss)
                         :Milliseconds (/ (double (- (. System (nanoTime)) startTime))
                                          1000000.0)
                         :Strategy (:strategy ss)
                         :StrategyCompute (:compute (:resources ss))
                         :StrategyMilliseconds (:milliseconds (:resources ss))
                         :StrategyMemory (:memory (:resources ss))))})))




