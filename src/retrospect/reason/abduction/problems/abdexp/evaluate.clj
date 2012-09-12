(ns retrospect.reason.abduction.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata hyp]
  (cond (= :kb (:type hyp)) true
        (= :expl (:type hyp))
        (if (= (:value hyp) ((:true-values-map truedata) (:vertex hyp))) true false)
        (= :observation (:type hyp))
        (if ((:true-obs truedata) (:vertex hyp)) true false)
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
  (let [metrics
        (for [ep (filter :decision-point (flatten-est est))]
          (let [ws (:workspace ep)
                acc (set (map #(lookup-hyp ws %) (get (:accepted ws) :expl)))
                not-acc (set/difference (set (map #(lookup-hyp ws %)
                                                (get (:hypotheses ws) :expl)))
                                        acc)
                [tp tn fp fn] (tp-tn-fp-fn (:true-values-map truedata) acc not-acc)]
            ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
            {:TP tp :TN tn :FP fp :FN fn
             :TPR (if (= 0 (+ tp fn)) 1.0 (/ (double tp) (double (+ tp fn))))
             :FPR (if (= 0 (+ fp tn)) 1.0 (/ (double fp) (double (+ fp tn))))
             :F1 (if (= 0 (+ tp fp fn)) 1.0 (/ (double (* 2.0 tp))
                                               (double (+ (* 2.0 tp) fp fn))))
             :TPRatio (if (empty? (:true-values-map truedata)) 1.0
                          (/ (double tp) (double (count (:true-values-map truedata)))))
             :Prec (if (= 0 (+ tp fp)) 1.0 (/ (double tp) (double (+ tp fp))))}))]
    (merge (last metrics)
           {:MinPrec (apply min (map :Prec metrics))
            :MinTPRatio (apply min (map :TPRatio metrics))
            :AvgPrec (/ (reduce + (map :Prec metrics)) (count metrics))
            :AvgTPRatio (/ (reduce + (map :TPRatio metrics)) (count metrics))})))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :TPRatio :Prec
                   :MinPrec :MinTPRatio :AvgPrec :AvgTPRatio])))

(defn stats
  [truedata ors time-now])
