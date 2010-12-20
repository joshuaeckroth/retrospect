(ns simulator.meta.hypotheses
  (:use [simulator.hypotheses :only [Hypothesis]])
  (:use [simulator.workspaces :only
         [add-hyp measure-decision-confidence force-acceptance
          clear-decision reset-confidences-to-apriori]])
  (:use [simulator.meta.actions])
  (:use [simulator.explain :only [explain-recursive]])
  (:use [simulator.confidences])
  (:use [clojure.set :as set :only [difference]]))

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
        (add-accurate-decision-hyp ep-state-hyp))))