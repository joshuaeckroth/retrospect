(ns simulator.meta.hypotheses
  (:use [simulator.hypotheses :only [Hypothesis]])
  (:use [simulator.workspaces :only
         [add-hyp measure-decision-confidence force-acceptance
          clear-decision reset-confidences-to-apriori]])
  (:use [simulator.meta.actions])
  (:use [simulator.strategies.composite])
  (:use [simulator.strategies.explain :only [explain-recursive]])
  (:use [simulator.confidences]))

(defrecord EpistemicStateHypothesis [ep-state]
  Hypothesis
  (get-id [_] "ep-state")
  (get-apriori [_] NEUTRAL))

(defrecord MetaHypothesis [id apriori action]
  Hypothesis
  (get-id [_] id)
  (get-apriori [_] apriori))

(defn generate-ep-state-hyp
  [ep-state]
  (EpistemicStateHypothesis. ep-state))

(defn add-change-single-strategy-hyps
  [workspace or-state ep-state-hyp]
  (let [ws-clone (reset-confidences-to-apriori
                  (clear-decision (:workspace (:ep-state or-state))))
        ws-new (fn [s] (explain-recursive ws-clone (:funcs (get strategy-info s))))
        hyps (map (fn [s]
                    (let [ws (ws-new s)]
                      (MetaHypothesis.
                       (format "MH:%s" s)
                       (measure-decision-confidence ws)
                       (partial change-strategy s ws))))
                  strategies)]
    (reduce (fn [ws h] (add-hyp ws h [ep-state-hyp])) workspace hyps)))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  (let [apriori (measure-decision-confidence (:workspace (:ep-state ep-state-hyp)))]
    (add-hyp workspace
             (MetaHypothesis. "MH:do-nothing" apriori do-nothing)
             [ep-state-hyp])))

(defn generate-meta-hypotheses
  [workspace or-state]
  (let [ep-state-hyp (generate-ep-state-hyp (:ep-state or-state))]
    (-> workspace
        (add-hyp ep-state-hyp [])
        (force-acceptance ep-state-hyp)
        (add-accurate-decision-hyp ep-state-hyp)
        (add-change-single-strategy-hyps or-state ep-state-hyp))))