(ns samre.meta.meta
  (:use [samre.confidences]))

(defn have-enough-meta-hyps
  [workspace]
  (some #(and (not= :ep-state (:id %)) (= NEUTRAL (:apriori %)))
        (vals (:hyps workspace))))

