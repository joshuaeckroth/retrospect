(ns retrospect.meta.meta
  (:use [retrospect.confidences])
  (:use [retrospect.workspaces :only [hyp-conf get-hyps]]))

(defn have-enough-meta-hyps
  [workspace]
  false #_(some #(and (not= :meta-ep (:type %)) (<= PLAUSIBLE (hyp-conf workspace %)))
                (get-hyps workspace)))

