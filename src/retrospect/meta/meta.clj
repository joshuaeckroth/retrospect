(ns retrospect.meta.meta
  (:use [retrospect.confidences])
  (:use [retrospect.workspaces :only [hyp-conf get-hyps]]))

(defn have-enough-meta-hyps
  [workspace]
  #_(some #(and (not= :meta-ep (:type %)) (<= 0.8 (hyp-conf workspace %)))
          (get-hyps workspace))
  false)

