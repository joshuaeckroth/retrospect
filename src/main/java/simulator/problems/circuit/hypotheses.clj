(ns simulator.problems.circuit.hypotheses
  (:use [clojure.set])
  (:use [simulator.strategies :only (add-hyp)])
  (:use [simulator.problems.circuit.circuit]))

(defrecord DiscrepancyHyp [id apriori input-vals ivstr index output expected observed]
  Object
  (toString [_] (format (str "DiscrepancyHyp %s (a=%.2f) given input vals: "
                             "%s, output %d should equal '%s' but '%s' was observed")
                        id apriori ivstr output
                        (str expected) (str observed))))

(defn make-disc-hyp-id
  [i oi exp obs]
  (format "DH%d%d" i oi))

(defn generate-discrepancy-hyps
  [gates wiring input-vals]
  "There is exactly one discrepancy hyp for each difference."
  (let [differences (find-differences gates wiring input-vals)]
    (map (fn [[i oi exp obs]]
           (DiscrepancyHyp. (make-disc-hyp-id i oi exp obs) 1.0
                            input-vals (format-input-vals input-vals gates)
                            i oi exp obs))
         differences)))

(defn find-discrepancies-explained
  [gate-id disc-hyps gates wiring]
  "A discrepancy is explained if the gate in question is in the parentage of
   the output identified by the discrepancy."
  (set (filter (fn [d-h] (some #(= gate-id %)
                               (conj (find-all-gate-input-gates (:index d-h) gates wiring)
                                     (:index d-h))))
               disc-hyps)))

(defrecord BrokenGateHyp [id apriori gate-id]
  Object
  (toString [_] (format "BrokenGateHyp %s (a=%.2f) gate %d is broken"
                        id apriori gate-id)))

(defn make-hyp-id
  [gate-id]
  (format "BGH%d" gate-id))

(defn generate-singular-hyps
  [gates wiring input-vals params]
  (let [disc-hyps (generate-discrepancy-hyps gates wiring input-vals)
        implicated (apply union (find-implicated-gates gates wiring input-vals))]
    (for [gate-id implicated]
      {:hyp (BrokenGateHyp. (make-hyp-id gate-id)
                            (double (/ (:ProbBroken params) 100)) gate-id)
       :explains (find-discrepancies-explained gate-id disc-hyps gates wiring)})))

(defn generate-hyps
  [strat-state time gates wiring input-vals params]
  (let [sing-hyps (generate-singular-hyps gates wiring input-vals params)]
    (reduce (fn [ss {hyp :hyp explains :explains}]
              (add-hyp ss time hyp explains (:apriori hyp)
                       (format (str "Hypothesizing (apriori=%.2f) that %s "
                                    "explains %s")
                               (:apriori hyp) (str hyp)
                               (apply str (interpose "; " (map str explains))))))
            strat-state sing-hyps)))

