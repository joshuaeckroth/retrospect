(ns samre.meta.meta
  (:use [samre.confidences]))

(defn have-enough-meta-hyps
  [hyps]
  (some #(and (not= :ep-state (:id %)) (= VERY-PLAUSIBLE (:apriori %))) hyps))