(ns simulator.meta.hypotheses
  (:use [simulator.hypotheses :only [Hypothesis]])
  (:use [simulator.workspaces :only
         [add-hyp measure-decision-confidence]])
  (:use [simulator.meta.actions])
  (:use [simulator.confidences]))

(defrecord EpistemicStateHypothesis [ep-state]
  Hypothesis
  (get-id [_] "ep-state")
  (get-apriori [_] VERY-PLAUSIBLE))

(defn generate-ep-state-hyp
  [ep-state]
  (EpistemicStateHypothesis. ep-state))

(defrecord MetaHypothesis [id apriori desc action]
  Hypothesis
  (get-id [_] id)
  (get-apriori [_] apriori))

(defn generate-accurate-decision
  [ep-state-hyp]
  (let [apriori (measure-decision-confidence (:ep-state ep-state-hyp))]
    (MetaHypothesis. "X" apriori "Decision is accurate." do-nothing)))

(defn generate-meta-hypotheses
  [workspace ep-state-hyp]
  (add-hyp workspace (generate-accurate-decision ep-state-hyp) [ep-state-hyp]))