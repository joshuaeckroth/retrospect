(ns retrospect.meta.meta
  (:use [retrospect.confidences])
  (:use [retrospect.workspaces :only [hyp-conf get-hyps]]))

(defn have-enough-meta-hyps
  [workspace]
  (some #(and (not= :ep-state (:type %)) (= NEUTRAL (hyp-conf workspace %)))
        (get-hyps workspace)))

