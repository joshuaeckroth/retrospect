(ns retrospect.problems.abdexp.hypotheses
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.math.combinatorics :only [subsets]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp new-composite]])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [geppetto.random])
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
  [expgraph bn observed-vertex-values parent-comb v val]
  (cond (= "prior" (:HypScores state/params))
        (prob expgraph v val parent-comb)
        (and (= "posterior" (:HypScores state/params))
             (not-empty parent-comb))
        (let [background (conj (filter #(not ((set parent-comb) %)) observed-vertex-values) [v val])]
          (unobserve-all bn)
          (observe-seq bn background)
          (get-posterior bn parent-comb))
        (and (= "posterior" (:HypScores state/params))
             (empty? parent-comb))
        (do
          (unobserve-all bn)
          (observe-seq bn observed-vertex-values)
          (get-posterior bn [[v val]]))
        :else 1.0))

(defn make-sensor-hyp
  [expgraph bn observed-vertex-values [v val]]
  (new-hyp "Obs" :observation :observation
           (make-score expgraph bn observed-vertex-values [] v val)
           (not-empty (explainers expgraph v)) ;; top leaves do not need to be explained
           [:observation] (partial hyps-conflict? expgraph)
           [] (format "Observed %s=%s" v val) (format "Observed %s=%s" v val)
           {:vertex v :value val}))

(defn make-sensor-hyps
  "Pick out the hyps that have been observed."
  [sensors time-prev time-now accepted hypotheses]
  (if (= time-prev time-now) []
      (let [kb (get-kb accepted)
            bn (:bayesnet kb)
            expgraph (:expgraph kb)
            ;; only :expl are "observed" here because :observation types
            ;; may not be believed, or may conflict with beliefs
            observed-vertex-values (map (fn [h] [(:vertex h) (:value h)]) (get :expl accepted))
            mk-fn (partial make-sensor-hyp expgraph bn observed-vertex-values)]
        (let [sens (set (mapcat #(sensed-at (first sensors) %)
                                (range time-prev (inc time-now))))]
          (map mk-fn sens)))))

(defn make-explainer
  [bn expgraph observed-hyps observed-vertex-values unexp-hyp pv pval]
  (let [score (make-score expgraph bn observed-vertex-values
                          [[pv pval]] (:vertex unexp-hyp) (:value unexp-hyp))]
    (new-hyp "Expl" :expl :expl score
             (not-empty (explainers expgraph pv))
             [:expl] (partial hyps-conflict? expgraph)
             (set (conj (map :contents (filter (fn [obs] (explains? expgraph pv (:vertex obs))) observed-hyps))
                        (:contents unexp-hyp)))
             (format "%s=%s" pv pval)
             (format "%s=%s" pv pval)
             {:vertex pv :value pval})))

(defn make-explainer-hyps
  [bn expgraph observed-hyps observed-vertex-values unexp-hyp]
  (let [v (:vertex unexp-hyp)
        val (:value unexp-hyp)]
    ;; if an observation came in on a expl we already believe, update that expl
    (if (and (= :observation (:type unexp-hyp))
             (some #(and (= :expl (:type %))
                         (= (:vertex unexp-hyp) (:vertex %)) (= (:value unexp-hyp) (:value %)))
                   observed-hyps))
      (let [expl-hyp (first (filter #(and (= :expl (:type %))
                                          (= (:vertex unexp-hyp) (:vertex %)) (= (:value unexp-hyp) (:value %)))
                                    observed-hyps))]
        [(update-in expl-hyp [:explains] conj (:contents unexp-hyp))])
      ;; else, not an observation that matches an accepted expl
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
                        parent-combs (gen-parent-combinations parent-vals)]
                    (if (:OnlySingleExplainers state/params)
                      ;; build a single explainer for each parent-comb
                      (map (fn [parent-comb]
                             (let [[pv pval] (first parent-comb)]
                               (make-explainer bn expgraph observed-hyps observed-vertex-values unexp-hyp pv pval)))
                           parent-combs)
                      ;; build a composite of several parent-combs
                      (map (fn [parent-comb]
                             (if (= 1 (count parent-comb))
                               ;; don't make a composite if there is only one vertex-value pair
                               (let [[pv pval] (first parent-comb)]
                                 (make-explainer bn expgraph observed-hyps observed-vertex-values unexp-hyp pv pval))
                               ;; make a composite if there are multiple vertex-value pairs
                               (let [hyps (map (fn [[pv pval]]
                                                 (make-explainer bn expgraph observed-hyps observed-vertex-values unexp-hyp pv pval))
                                               parent-comb)
                                     score (make-score expgraph bn observed-vertex-values
                                                       parent-comb v val)]
                                 (new-composite "ExplComp" :expl :expl-composite
                                                score [(:contents unexp-hyp)]
                                                (str/join "," (map (fn [[pv pval]]
                                                                     (format "%s=%s" pv pval))
                                                                   parent-comb))
                                                (format "Composite of:\n%s"
                                                        (str/join "\n" (map str hyps)))
                                                {:parent-comb parent-comb} hyps))))
                           parent-combs))))
                expl-sets)))))

(defn hypothesize
  [unexp accepted hypotheses time-now]
  (let [kb (get-kb accepted)
        bn (:bayesnet kb)
        expgraph (:expgraph kb)
        observed-hyps (filter #(or (= :observation (:type %)) (= :expl (:subtype %))) (:all accepted))
        observed-vertex-values (map (fn [h] [(:vertex h) (:value h)]) observed-hyps)
        new-expl-hyps (mapcat #(make-explainer-hyps bn expgraph observed-hyps observed-vertex-values %) unexp)]
    new-expl-hyps))

(comment
  ;; composite explainers have already been checked for conflicts with observed-vertex-values
  (filter #(or (= :expl-composite (:type %))
               (not (any-vertex-values-conflict? expgraph (:vertex %) (:value %) observed-vertex-values)))
          ))




