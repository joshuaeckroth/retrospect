(ns retrospect.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate])
  (:use [retrospect.epistemicstates :only [cur-ep decision-points]])
  (:use [retrospect.reason.abduction.workspace :only
         [hypotheses accepted rejected rejection-reason calc-doubt]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [geppetto.random])
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
        metrics (for [ep (decision-points est)]
                  (let [ws (:workspace ep)
                        acc (filter #(= :expl (:subtype %)) (:expl (accepted ws)))
                        acc-vertices (set (map :vertex acc))
                        obs-vertices (set (map :vertex (:observation (hypotheses ws))))
                        rej (filter #(= :expl (:subtype %)) (:expl (rejected ws)))
                        mpe-map (let [bn2 (copy-net bn)]
                                  (unobserve-all bn2)
                                  ;; shuffle/sort so that duplication noise (conflicting vals for same var)
                                  ;; are "observed" in the same order every time
                                  (observe-seq bn2 (my-shuffle (sort (map (fn [obs] [(:vertex obs) (:value obs)])
                                                                          (:observation (hypotheses ws))))))
                                  (absorb-vertices bn2 (filter (fn [v] (not (acc-vertices v)))
                                                               (vertices expgraph)))
                                  (:states (most-probable-explanation bn2)))
                        mpe-acc (for [[v val] mpe-map] {:vertex v :value val})
                        true-values-map (:true-values-map truedata)
                        [tp tn fp fn] (tp-tn-fp-fn true-values-map acc rej)
                        prec-recall (calc-prec-recall tp tn fp fn (count true-values-map) "")
                        [mtp mtn mfp mfn] (tp-tn-fp-fn true-values-map mpe-acc [])
                        mpe-prec-recall (calc-prec-recall mtp mtn mfp mfn (count true-values-map) "")]
                    (assoc prec-recall :MPEAccuracy (:Accuracy mpe-prec-recall))))]
    (merge (compute-complexity expgraph)
           (last metrics)
           {:AvgPrec (avg (map :Prec metrics))
            :AvgRecall (avg (map :Recall metrics))
            :AvgAccuracy (avg (map :Accuracy metrics))
            :AvgF1 (avg (map :F1 metrics))
            :AvgTPR (avg (map :TPR metrics))
            :AvgFPR (avg (map :FPR metrics))
            :AvgTNR (avg (map :TNR metrics))
            :AvgPPV (avg (map :PPV metrics))
            :AvgNPV (avg (map :NPV metrics))
            :AvgFDR (avg (map :FDR metrics))})))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:Prec :AvgPrec :Recall :AvgRecall :F1 :AvgF1
                   :TPR :FPR :AvgTPR :AvgFPR :TNR :AvgTNR :PPV :AvgPPV
                   :NPV :AvgNPV :FDR :AvgFDR :Accuracy :AvgAccuracy])))

(defn stats
  [truedata ors time-now])
