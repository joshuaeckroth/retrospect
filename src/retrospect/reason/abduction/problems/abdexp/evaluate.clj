(ns retrospect.reason.abduction.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase calc-prec-coverage avg]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp calc-doubt]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata hyp]
  (cond (= :kb (:type hyp)) true
        (= :expl-composite (:type hyp))
        (every? #(true-hyp? truedata %) (:hyps hyp))
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
        _ (println "truedata:" (:test truedata))
        metrics
        (for [ep (filter :decision-point (flatten-est est))]
          (let [ws (:workspace ep)
                observed (apply concat (take (:time ep) (:test truedata)))
                _ (println "time:" (:time ep))
                _ (println "observed:" observed)
                _ (println "downstream all:" (expl-ancestors expgraph (map first observed)))
                _ (println "downstream true:" (select-keys (:true-values-map truedata)
                                                           (expl-ancestors expgraph (map first observed))))
                acc (set (map #(lookup-hyp ws %) (get (:accepted ws) :expl)))
                acc-vertex-values (set (map (fn [h] [(:vertex h) (:value h)]) acc))
                not-acc (set/difference (set (map #(lookup-hyp ws %)
                                                (get (:hypotheses ws) :expl)))
                                        acc)
                [tp tn fp fn] (tp-tn-fp-fn (select-keys (:true-values-map truedata)
                                                        (expl-ancestors expgraph (map first observed)))
                                           acc not-acc)
                prec-coverage (calc-prec-coverage tp tn fp fn (count (:true-values-map truedata)))
                probs (let [bn (:bayesnet truedata)]
                        (unobserve-all bn)
                        (observe-seq bn observed)
                        (let [prob (get-posterior bn acc-vertex-values)
                              {mpe :states mpe-prob :prob} (most-probable-explanation bn)
                              [etp etn efp efn] (tp-tn-fp-fn mpe acc not-acc)
                              mpe-prec-coverage (calc-prec-coverage etp etn efp efn (count mpe))
                              post-error (avg (for [h acc]
                                                (let [v (:vertex h)
                                                      vals (sort (values expgraph v))
                                                      post-map (zipmap vals (map #(get-posterior bn v %) vals))]
                                                  (- (post-map (:value h)) (apply max (map second (seq post-map)))))))
                              
                              
                              posteriors-acc (map #(get-posterior bn (first %) (second %))
                                                acc-vertex-values)]
                          {:Prob prob
                           :MPEPrec (:Prec mpe-prec-coverage)
                           :MPECoverage (:Coverage mpe-prec-coverage)
                           :MPEProb mpe-prob
                           :PostError post-error
                           :AbsConfPostDiff (Math/abs (- confidence (avg posteriors-acc)))}))]
            (merge prec-coverage probs)))]
    (merge (last metrics)
           {:MinPrec (apply min (map :Prec metrics))
            :MinCoverage (apply min (map :Coverage metrics))
            :MinProb (apply min (map :Prob metrics))
            :AvgPrec (avg (map :Prec metrics))
            :AvgCoverage (avg (map :Coverage metrics))
            :AvgProb (avg (map :Prob metrics))})))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :Coverage :Prec
                   :MinPrec :MinCoverage :AvgPrec :AvgCoverage :Prob
                   :MinProb :AvgProb])))

(defn stats
  [truedata ors time-now])
