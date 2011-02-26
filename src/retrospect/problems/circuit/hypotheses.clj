(ns samre.problems.circuit.hypotheses
  (:use [clojure.set])
  (:use [samre.types.hypotheses :only (Hypothesis get-hyp-id-str get-hyp-ids-str)])
  (:use [samre.confidences])
  (:use [samre.strategies :only (add-hyp force-acceptance)])
  (:use [samre.problems.circuit.circuit]))

(defrecord DiscrepancyHyp [id apriori input-vals ivstr index output expected observed]
  Hypothesis
  (get-id [_] id)
  (get-apriori [_] apriori)
  Object
  (toString [_] (format (str "DiscrepancyHyp %s (a=%d) given input vals: "
                             "%s, output %d should equal '%s' but '%s' was observed")
                        id apriori ivstr output
                        (str expected) (str observed))))

(defrecord BrokenGateHyp [id apriori gate-id]
  Hypothesis
  (get-id [_] id)
  (get-apriori [_] apriori)
  Object
  (toString [_] (format "BrokenGateHyp %s (a=%d) gate %d is broken"
                        id apriori gate-id)))

(defn make-disc-hyp-id
  [i oi exp obs]
  (format "DH%d%d" i oi))

(defn generate-discrepancy-hyps
  [gates wiring input-vals]
  "There is exactly one discrepancy hyp for each difference."
  (let [differences (find-differences gates wiring input-vals)]
    (map (fn [[i oi exp obs]]
           (DiscrepancyHyp. (make-disc-hyp-id i oi exp obs) VERY-PLAUSIBLE
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

(defn make-hyp-id
  [gate-id]
  (format "BGH%d" gate-id))

(defn generate-singular-hyps
  [gates wiring input-vals params]
  (let [disc-hyps (generate-discrepancy-hyps gates wiring input-vals)
        implicated (apply union (find-implicated-gates gates wiring input-vals))
        apriori (if (>= 0.5 (:ProbBroken params)) IMPLAUSIBLE PLAUSIBLE)]
    (for [gate-id implicated]
      {:hyp (BrokenGateHyp. (make-hyp-id gate-id)
                            apriori gate-id)
       :explains (find-discrepancies-explained gate-id disc-hyps gates wiring)})))

(defn generate-hyps
  [strat-state time gates wiring input-vals params]
  (let [sing-hyps (generate-singular-hyps gates wiring input-vals params)
        ss (reduce (fn [ss d-h]
                     (-> ss
                         (add-hyp time d-h #{}
                                  (format "Hypothesizing discrepancy %s"
                                          (get-hyp-id-str d-h)))
                         (force-acceptance time d-h
                                           (format "Accepting as fact discrepancy %s"
                                                   (get-hyp-id-str d-h)))))
                   strat-state (apply union (map :explains sing-hyps)))]    
    (reduce (fn [ss {hyp :hyp explains :explains}]
              (add-hyp ss time hyp explains
                       (format (str "Hypothesizing (a=%d) that %s "
                                    "explains %s")
                               (:apriori hyp) (get-hyp-id-str hyp)
                               (get-hyp-ids-str explains))))
            ss sing-hyps)))

