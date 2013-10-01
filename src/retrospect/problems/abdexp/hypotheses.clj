(ns retrospect.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.math.combinatorics :only [subsets]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp new-composite]])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.problems.abdexp.expgraph])
  (:require [retrospect.state :as state]))

(defn generate-kb
  "This function introduces the whole expgraph so that it is ready to
   use when reasoning starts."
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false [] nil [] "" ""
            {:expgraph (:expgraph training) :bayesnet (:bayesnet training)})])

(defn get-kb
  [accepted]
  (first (get accepted :kb)))

;; TODO
(defn update-kb
  [accepted unexplained hypotheses]
  [(get-kb accepted)])

(defn vertex-values-conflict?
  [expgraph v1 val1 v2 val2]
  (or (and (= v1 v2)
           (not= val1 val2))
      (conflicts? expgraph [v1 val1] [v2 val2])))

(defn any-vertex-values-conflict?
  [expgraph v1 val1 other-vertex-values]
  (some (fn [[v2 val2]] (vertex-values-conflict? expgraph v1 val1 v2 val2))
     other-vertex-values))

(defn hyps-conflict?
  [expgraph hyp1 hyp2]
  (vertex-values-conflict?
   expgraph (:vertex hyp1) (:value hyp1) (:vertex hyp2) (:value hyp2)))

(defn make-score
  "Figure out the (approximate) probability of v=val given
   parents (parent-comb) and observed. Note that parent-comb may be
   empty, indicating we want the probability of v=val without mention
   of the parents."
  [expgraph bn observed parent-comb v val]
  (cond (= "prior" (:HypScores state/params))
        (prob expgraph v val parent-comb)
        (and (= "posterior" (:HypScores state/params))
             (not-empty parent-comb))
        (let [background (conj (filter #(not ((set parent-comb) %)) observed) [v val])]
          (unobserve-all bn)
          (observe-seq bn background)
          (get-posterior bn parent-comb))
        (and (= "posterior" (:HypScores state/params))
             (empty? parent-comb))
        (do
          (unobserve-all bn)
          (observe-seq bn observed)
          (get-posterior bn [[v val]]))
        :else 1.0))

(defn make-sensor-hyps
  "Pick out the hyps that have been observed."
  [sensors time-prev time-now accepted hypotheses anomalies]
  (if (= time-prev time-now) []
      (let [kb (get-kb accepted)
            bn (:bayesnet kb)
            expgraph (:expgraph kb)
            ;; only :expl are "observed" here because :observation types
            ;; may not be believed, or may conflict with beliefs
            observed (map (fn [h] [(:vertex h) (:value h)]) (get :expl accepted))
            ;; figure out what the sensor has observed
            sens-observed (set (mapcat #(sensed-at (first sensors) %)
                                       (range time-prev (inc time-now))))]
        (for [[v val] sens-observed]
          (new-hyp "Obs" :observation :observation
                   (make-score expgraph bn observed [] v val)
                   true [:observation] (partial hyps-conflict? expgraph)
                   [] (format "Observed %s=%s" v val) (format "Observed %s=%s" v val)
                   {:vertex v :value val})))))

(defn make-explainer
  [bn expgraph observed unexp-hyp pv pval]
  (let [score (make-score expgraph bn observed
                          [[pv pval]] (:vertex unexp-hyp) (:value unexp-hyp))]
    (new-hyp "Expl" :expl :expl score
             (not-empty (explainers expgraph pv))
             [:expl] (partial hyps-conflict? expgraph)
             [(:contents unexp-hyp)]
             (format "%s=%s" pv pval)
             (format "%s=%s" pv pval)
             {:vertex pv :value pval})))

(defn make-explainer-hyps
  [bn expgraph observed unexp-hyp]
  (let [v (:vertex unexp-hyp)
        val (:value unexp-hyp)]
    (if (= :observation (:type unexp-hyp))
      (let [score (make-score expgraph bn observed [] v val)]
        [(new-hyp "Expl" :expl :expl score
                  (not-empty (explainers expgraph v))
                  [:expl] (partial hyps-conflict? expgraph)
                  [(:contents unexp-hyp)]
                  (format "%s=%s" v val)
                  (format "%s=%s" v val)
                  {:vertex v :value val})])
      ;; else, not an observation
      (let [expl (explainers expgraph v)
            expl-sets (cond (:OnlySingleExplainers state/params)
                            (for [e expl] [e]) ;; a single parent state is enough to explain
                            (:OnlyCompleteExplainers state/params)
                            [expl]
                            :else
                            ;; try all subsets of parent states
                            (filter not-empty (subsets expl)))]
        (mapcat (fn [expl-set]
                  (let [parent-vals (map (fn [pv] (map (fn [pval] [pv pval])
                                                       (sort (values expgraph pv))))
                                         (sort expl-set))
                        parent-combs (gen-parent-combinations parent-vals)
                        non-conf-parent-combs (filter (fn [parent-comb]
                                                        (not-any? (fn [[pv pval]]
                                                                    (any-vertex-values-conflict?
                                                                     expgraph pv pval observed))
                                                                  parent-comb))
                                                      parent-combs)]
                    (if (:OnlySingleExplainers state/params)
                      ;; build a single explainer for each parent-comb
                      (map (fn [parent-comb]
                             (let [[pv pval] (first parent-comb)]
                               (make-explainer bn expgraph observed unexp-hyp pv pval)))
                           non-conf-parent-combs)
                      ;; build a composite of several parent-combs
                      (map (fn [parent-comb]
                             (if (= 1 (count parent-comb))
                               ;; don't make a composite if there is only one vertex-value pair
                               (let [[pv pval] (first parent-comb)]
                                 (make-explainer bn expgraph observed unexp-hyp pv pval))
                               ;; make a composite if there are multiple vertex-value pairs
                               (let [hyps (map (fn [[pv pval]]
                                                 (make-explainer bn expgraph observed unexp-hyp pv pval))
                                               parent-comb)
                                     score (make-score expgraph bn observed
                                                       parent-comb v val)]
                                 (new-composite "ExplComp" :expl :expl-composite
                                                score [(:contents unexp-hyp)]
                                                (str/join "," (map (fn [[pv pval]]
                                                                     (format "%s=%s" pv pval))
                                                                   parent-comb))
                                                (format "Composite of:\n%s"
                                                        (str/join "\n" (map str hyps)))
                                                {:parent-comb parent-comb} hyps))))
                           non-conf-parent-combs))))
                expl-sets)))))

(defn hypothesize
  [unexp accepted hypotheses time-now]
  (let [kb (get-kb accepted)
        bn (:bayesnet kb)
        expgraph (:expgraph kb)
        ;; only :expl are "observed" here because :observation types
        ;; may not be believed, or may conflict with beliefs
        observed (map (fn [h] [(:vertex h) (:value h)]) (filter #(= :expl (:subtype %)) (:expl accepted)))]
    (mapcat #(make-explainer-hyps bn expgraph observed %) unexp)))


