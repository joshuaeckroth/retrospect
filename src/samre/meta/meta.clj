(ns samre.meta.meta
  (:use [samre.confidences]))

(defn have-enough-meta-hyps
  [workspace]
  (some #(and (not= :ep-state (:id %)) (= PLAUSIBLE (:apriori %)))
        (vals (:hyps workspace))))