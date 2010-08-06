(ns simulator.evaluator)

(defn evaluate
  [truestate strat-state]
  (count (set/intersection (set (:events truestate)) (set (:events strat-state)))))

(defn correct?
  [truestate event]
  (some #(= event %) (:events truestate)))