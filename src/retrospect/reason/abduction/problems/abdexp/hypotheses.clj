(ns retrospect.reason.abduction.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.contrib.combinatorics :only [subsets]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp new-composite]])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.problems.abdexp.expgraph])
  (:require [retrospect.state :as state]))

(defn generate-kb
  "This function introduces the whole expgraph so that it is ready to
   use when reasoning starts."
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil [] "" ""
            {:expgraph (:expgraph training) :bayesnet (:bayesnet training)})])

(defn get-kb
  [accepted lookup-hyp]
  (first (filter :expgraph (map lookup-hyp (get accepted :kb)))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn hyps-conflict?
  [expgraph hyp1 hyp2]
  (and (not= (:id hyp1) (:id hyp2))
       (not= (:observation (:type hyp1)))
       (not= (:observation (:type hyp2)))
       (or (and (= (:type hyp1) (:type hyp2))
                (= (:vertex hyp1) (:vertex hyp2))
                (not= (:value hyp1) (:value hyp2)))
           (conflicts? expgraph
                       [(:vertex hyp1) (:value hyp1)]
                       [(:vertex hyp2) (:value hyp2)]))))

(defn make-sensor-hyps
  "Pick out the hyps that have been observed."
  [sensors time-prev time-now accepted all-hyps lookup-hyp]
  (if (= time-prev time-now) []
      (let [kb (get-kb accepted lookup-hyp)
            bn (:bayesnet kb)
            expgraph (:expgraph kb)
            ;; only :expl are "observed" here because :observation types
            ;; may not be believed, or may conflict with beliefs
            observed (map (fn [h] [(:vertex h) (:value h)]) (map lookup-hyp (:expl accepted)))
            ;; figure out what the sensor has observed
            sens-observed (set (mapcat #(sensed-at (first sensors) %)
                                       (range (inc time-now))))]
        (for [[v val] sens-observed]
          (new-hyp "Obs" :observation :observation 1.0 true nil
                   [] (format "Observed %s=%s" v val) (format "Observed %s=%s" v val)
                   {:vertex v :value val})))))

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

(defn make-explainer-for-composite
  [bn expgraph observed unexp-hyp pv pval]
  (let [score (make-score expgraph bn observed
                          [[pv pval]] (:vertex unexp-hyp) (:value unexp-hyp))]
    (new-hyp "Expl" :expl :expl score
             (not-empty (explainers expgraph pv))
             #(hyps-conflict? expgraph %1 %2)
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
                  #(hyps-conflict? expgraph %1 %2)
                  [(:contents unexp-hyp)]
                  (format "%s=%s" v val)
                  (format "%s=%s" v val)
                  {:vertex v :value val})])
      ;; else, not an observation
      (let [expl (explainers expgraph v)
            expl-sets (filter not-empty (subsets expl))]
        (mapcat (fn [expl-set]
                  (let [parent-vals (map (fn [pv] (map (fn [pval] [pv pval])
                                                  (sort (values expgraph pv))))
                                       (sort expl-set))
                        parent-combs (gen-parent-combinations parent-vals)]
                    (map (fn [parent-comb]
                         (let [hyps (map (fn [[pv pval]]
                                         (make-explainer-for-composite
                                          bn expgraph observed unexp-hyp pv pval))
                                       parent-comb)
                               score (make-score expgraph bn observed
                                                 parent-comb v val)]
                           (new-composite "ExplComp" :expl-composite :expl-composite
                                          score [(:contents unexp-hyp)]
                                          (str/join "," (map (fn [[pv pval]]
                                                             (format "%s=%s" pv pval))
                                                           parent-comb))
                                          (format "Composite of:\n%s"
                                             (str/join "\n" (map str hyps)))
                                          {:parent-comb parent-comb} hyps)))
                       parent-combs)))
                expl-sets)))))

(defn hypothesize
  [unexp-hyps accepted all-hyps lookup-hyp time-now]
  (let [kb (get-kb accepted lookup-hyp)
        bn (:bayesnet kb)
        expgraph (:expgraph kb)
        ;; only :expl are "observed" here because :observation types
        ;; may not be believed, or may conflict with beliefs
        observed (map (fn [h] [(:vertex h) (:value h)]) (map lookup-hyp (:expl accepted)))]
    (mapcat #(make-explainer-hyps bn expgraph observed %) unexp-hyps)))
