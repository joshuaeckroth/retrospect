(ns retrospect.reason.abduction.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase calc-prec-tpratio avg]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp calc-doubt]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.javabayes])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata hyp]
  (cond (= :kb (:type hyp)) true
        (or (= :expl (:type hyp)) (= :observation (:type hyp)))
        (if (= (:value hyp) ((:true-values-map truedata) (:vertex hyp))) true false)
        :else false))

(defn count-matches
  [true-values-map hyps]
  (count (filter #(= (:value %) (true-values-map (:vertex %))) hyps)))

(defn tp-tn-fp-fn
  [true-values-map acc not-acc]
  (if (empty? true-values-map) [0 0 0 0]
      (let [true-pos (count-matches true-values-map acc)
            false-pos (- (count acc) true-pos)
            false-neg (count-matches true-values-map not-acc)
            true-neg (- (count not-acc) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn evaluate
  [truedata est]
  (let [expgraph (:expgraph truedata)
        confidence (- 1.0 (doubt-aggregate est))
        metrics
        (for [ep (filter :decision-point (flatten-est est))]
          (let [ws (:workspace ep)
                acc (set (map #(lookup-hyp ws %) (get (:accepted ws) :expl)))
                acc-vertex-values (set (map (fn [h] [(:vertex h) (:value h)]) acc))
                acc-vertex-alt-values (set/difference
                                       (set (mapcat (fn [h] (for [val (values expgraph (:vertex h))]
                                                           [(:vertex h) val]))
                                                  acc))
                                       acc-vertex-values)
                not-acc (set/difference (set (map #(lookup-hyp ws %)
                                                (get (:hypotheses ws) :expl)))
                                        acc)
                [tp tn fp fn] (tp-tn-fp-fn (:true-values-map truedata) acc not-acc)
                prec-tpratio (calc-prec-tpratio tp tn fp fn (count (:true-values-map truedata)))
                probs (when-let [bn (:bayesnet truedata)]
                        (unobserve-all bn)
                        (observe-seq bn (apply concat (:test truedata)))
                        (let [acc-probs (map #(get-posterior-marginal
                                             bn (:vertex %) (:value %)) acc)
                              acc-alt-probs (map #(get-posterior-marginal
                                                 bn (first %) (second %))
                                               acc-vertex-alt-values)
                              prob (cond (and (empty? acc-probs) (not-empty acc-alt-probs))
                                         (- (avg acc-alt-probs))
                                         (and (not-empty acc-probs) (not-empty acc-alt-probs))
                                         (- (avg acc-probs) (avg acc-alt-probs))
                                         :else 0.0)
                              expl (get-explanation bn)
                              [etp etn efp efn] (tp-tn-fp-fn expl acc not-acc)
                              prec-tpratio (calc-prec-tpratio etp etn efp efn (count expl))
                              posteriors-acc (map #(get-posterior-marginal bn (first %) (second %))
                                                acc-vertex-values)]
                          {:Prob prob :ExplPrec (:Prec prec-tpratio) :ExplTPRatio (:TPRatio prec-tpratio)
                           :AbsConfPostDiff (Math/abs (- confidence (avg posteriors-acc)))}))]
            (merge prec-tpratio (or probs {}))))]
    (merge (last metrics)
           {:MinPrec (apply min (map :Prec metrics))
            :MinTPRatio (apply min (map :TPRatio metrics))
            :AvgPrec (avg (map :Prec metrics))
            :AvgTPRatio (avg (map :TPRatio metrics))})))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :TPRatio :Prec
                   :MinPrec :MinTPRatio :AvgPrec :AvgTPRatio :Prob])))

(defn stats
  [truedata ors time-now])
