(ns simulator.meta.hypotheses
  (:require [simulator workspaces])
  (:import [simulator.workspaces Hypothesis])
  (:use [simulator.workspaces :only
         [add-hyp measure-decision-confidence force-acceptance
          clear-decision reset-confidences-to-apriori explain]])
  (:use [simulator.meta.actions])
  (:use [simulator.confidences])
  (:use [clojure.set :as set :only [difference]]))

(defrecord EpistemicStateHypothesis [ep-state])

(defrecord MetaHypothesis [id apriori action])

(defn generate-ep-state-hyp
  [ep-state]
  (Hypothesis. :ep-state :meta VERY-PLAUSIBLE VERY-PLAUSIBLE
               [] (constantly []) (constantly []) identity
               (constantly false)
               (constantly "ep-state") nil))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  (let [apriori (measure-decision-confidence (:workspace (:ep-state ep-state-hyp)))
        hyp (Hypothesis. :MH-dec-accurate :meta apriori apriori
                         [(:id ep-state-hyp)] (constantly []) (constantly [])
                         identity (constantly false)
                         (constantly "Decision is accurate") nil)]
    (add-hyp workspace hyp)))

(defn generate-meta-hypotheses
  [workspace or-state]
  (let [ep-state-hyp (generate-ep-state-hyp (:ep-state or-state))]
    (-> workspace
        (add-hyp ep-state-hyp)
        (force-acceptance ep-state-hyp)
        (add-accurate-decision-hyp ep-state-hyp))))