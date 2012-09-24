(ns retrospect.reason.abduction.problems.tracking.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase calc-prec-tpratio]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.problems.tracking.movements :only [moves-match?]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata hyp]
  (cond (= :movement (:type hyp))
        (if (some #(moves-match? (:mov hyp) %) (:all-moves truedata)) true false)
        ;; check for sensor noise
        (= :observation (:type hyp))
        (if ((:all-xys truedata) {:x (:x (:det hyp)) :y (:y (:det hyp))
                                  :time (:time (:det hyp))})
          true false)))

(defn hyps-equal?
  [hyp1 hyp2]
  (if (not= (:type hyp1) (:type hyp2)) false
      (cond (= :movement (:type hyp1))
            (= (:mov hyp1) (:mov hyp2))
            :else false)))

(defn count-matches
  [true-movs movs]
  (count (filter (fn [m] (some #(moves-match? m %) true-movs)) movs)))

(defn tp-tn-fp-fn
  [true-movs acc-movs not-acc-movs]
  (if (empty? true-movs) [0 0 0 0]
      (let [true-pos (count-matches true-movs acc-movs)
            false-pos (- (count acc-movs) true-pos)
            false-neg (count-matches true-movs not-acc-movs)
            true-neg (- (count not-acc-movs) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn get-true-movements
  [truedata time-now]
  (set (filter #(and (:ot %) (<= (:time %) time-now))
          (:all-moves truedata))))

(defn evaluate
  [truedata est]
  (if (or (and (not training?) (not @batch))
          (and (not training?) (= (:Steps params) (:time (cur-ep est)))))
    (let [metrics
          (for [ep (filter :decision-point (flatten-est est))]
            (let [ws (:workspace ep)
                  time-now (:time ep)
                  true-movs (get-true-movements truedata time-now)
                  accepted (map #(lookup-hyp ws %) (:movement (:accepted ws)))
                  not-accepted (set/difference
                                (set (map #(lookup-hyp ws %)
                                        (:movement (:hypotheses ws))))
                                accepted)
                  acc-movs (map :mov accepted)
                  not-acc-movs (map :mov not-accepted)
                  [tp tn fp fn] (tp-tn-fp-fn true-movs acc-movs not-acc-movs)]
              (calc-prec-tpratio tp tn fp fn (count true-movs))))]
      (merge (last metrics)
             {:MinPrec (apply min (map :Prec metrics))
              :MinTPRatio (apply min (map :TPRatio metrics))
              :AvgPrec (/ (reduce + (map :Prec metrics)) (count metrics))
            :AvgTPRatio (/ (reduce + (map :TPRatio metrics)) (count metrics))}))
    {:TP 0 :TN 0 :FP 0 :FN 0 :TPR 0.0 :FPR 0.0 :F1 0.0 :TPRatio 0.0 :Prec 0.0
     :MinPrec 0.0 :MinTPRatio 0.0 :AvgPrec 0.0 :AvgTPRatio 0.0}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :TPRatio :Prec
                   :MinPrec :MinTPRatio :AvgPrec :AvgTPRatio])))

(defn training-stats
  [workspace false-accepted unexplained truedata time-now cycle])
