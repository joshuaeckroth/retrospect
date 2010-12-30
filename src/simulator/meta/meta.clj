(ns simulator.meta.meta
  (:use [simulator.confidences]))

(defn have-enough-meta-hyps
  [hyps]
  (some #(and (not= :ep-state (:id %)) (= VERY-PLAUSIBLE (:apriori %))) hyps))