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
                acc-top-nodes (filter (comp not :needs-explainer?) acc)
                acc-vertex-values (set (map (fn [h] [(:vertex h) (:value h)]) acc))
                rej (:expl (rejected ws))
                rej-top-nodes (filter (comp not :needs-explainer?) rej)
                top-nodes-truth (select-keys (:true-values-map truedata)
                                             (top-nodes expgraph))
                [tp tn fp fn] (tp-tn-fp-fn top-nodes-truth acc-top-nodes rej-top-nodes)
                prec-coverage (calc-prec-coverage tp tn fp fn (count top-nodes-truth))
                obs-coverage (/ (double
                                 (count (filter #(= (:value %) ((:true-values-map truedata)
                                                           (:vertex %)))
                                           ;; get hyps that explain an obs
                                           (filter #(let [e (:explains %)]
                                                 (and (= 1 (count e))
                                                      (= :observation (:type (first e)))))
                                              acc))))
                                (double (count observed)))
                {mpe :states} (most-probable-explanation bn)
                [etp etn efp efn] (tp-tn-fp-fn mpe acc rej)
                mpe-prec-coverage (calc-prec-coverage etp etn efp efn (count mpe))]
            {:MPEPrec (:Prec mpe-prec-coverage)
             :MPECoverage (:Coverage mpe-prec-coverage)
             :MPEF1 (:F1 mpe-prec-coverage)
             :Prec (:Prec prec-coverage)
             :Coverage (:Coverage prec-coverage)
             :F1 (:F1 prec-coverage)
             :ObsCoverage obs-coverage}))]
    (merge (last metrics)
           (compute-complexity expgraph)
           {:AvgMPEPrec (avg (map :MPEPrec metrics))
            :AvgMPECoverage (avg (map :MPECoverage metrics))
            :AvgMPEF1 (avg (map :MPEF1 metrics))
            :AvgPrec (avg (map :Prec metrics))
            :AvgCoverage (avg (map :Coverage metrics))
            :AvgF1 (avg (map :F1 metrics))
            :AvgObsCoverage (avg (map :ObsCoverage metrics))})))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:Prec :AvgPrec :Coverage :AvgCoverage :F1 :AvgF1 :ObsCoverage :AvgObsCoverage])))

(defn stats
  [truedata ors time-now])
