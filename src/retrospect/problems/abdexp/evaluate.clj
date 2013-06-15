(ns retrospect.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate])
  (:use [retrospect.epistemicstates :only [cur-ep decision-points]])
  (:use [retrospect.reason.abduction.workspace :only [accepted rejected calc-doubt]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata hyp]
  (cond (= :kb (:type hyp)) true
        (= :expl-composite (:subtype hyp))
        (every? #(true-hyp? truedata %) (:hyps hyp))
        (or (= :expl (:type hyp)) (= :observation (:type hyp)))
        (if (= (:value hyp) ((:true-values-map truedata) (:vertex hyp))) true false)
        :else false))

(defn count-matches
  [true-values-map hyps]
  (count (filter #(= (:value %) (true-values-map (:vertex %))) hyps)))

(defn tp-tn-fp-fn
  [true-values-map acc rej]
  (if (empty? true-values-map) [0 0 0 0]
      (let [true-pos (count-matches true-values-map acc)
            false-pos (- (count acc) true-pos)
            false-neg (count-matches true-values-map rej)
            true-neg (- (count rej) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn evaluate
  [truedata est]
  (let [expgraph (:expgraph truedata)
        bn (:bayesnet truedata)
        confidence (- 1.0 (doubt-aggregate est))
        metrics
        (for [ep (decision-points est)]
          (let [ws (:workspace ep)
                observed (apply concat (take (:time ep) (:test truedata)))
                _ (do (unobserve-all bn)
                      (observe-seq bn observed))
                acc (:expl (accepted ws))
                rej (:expl (rejected ws))
                {mpe :states} (most-probable-explanation bn)
                [etp etn efp efn] (tp-tn-fp-fn mpe acc rej)
                mpe-prec-coverage (calc-prec-coverage etp etn efp efn (count mpe))]
            {:Prec (:Prec mpe-prec-coverage)
             :Coverage (:Coverage mpe-prec-coverage)
             :F1 (:F1 mpe-prec-coverage)
             :TPR (:TPR mpe-prec-coverage)
             :FPR (:FPR mpe-prec-coverage)}))]
    (merge (last metrics)
           (compute-complexity expgraph)
           {:AvgPrec (avg (map :MPEPrec metrics))
            :AvgCoverage (avg (map :MPECoverage metrics))
            :AvgF1 (avg (map :MPEF1 metrics))
            :AvgTPR (avg (map :TPR metrics))
            :AvgFPR (avg (map :FPR metrics))})))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:Prec :AvgPrec :Coverage :AvgCoverage :F1 :AvgF1
                   :TPR :FPR :AvgTPR :AvgFPR])))

(defn stats
  [truedata ors time-now])
