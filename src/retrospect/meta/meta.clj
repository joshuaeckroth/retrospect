(ns retrospect.meta.meta
  (:use [retrospect.confidences]))

(defn have-enough-meta-hyps
  [workspace]
  (some #(and (not= :ep-state (:id %)) (= NEUTRAL (:apriori %)))
        (vals (:hyps workspace))))

