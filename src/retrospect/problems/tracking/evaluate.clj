(ns retrospect.problems.tracking.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate])
  (:use [retrospect.epistemicstates :only [cur-ep goto-cycle decision-points]])
  (:use [retrospect.reason.abduction.workspace :only [accepted rejected]])
  (:use [retrospect.problems.tracking.movements :only [moves-match?]])
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
          (for [ep (decision-points est)]
            (let [ws (:workspace ep)
                  time-now (:time ep)
                  true-movs (get-true-movements truedata time-now)
                  acc-movs (map :mov (:movement (accepted ws)))
                  rej-movs (map :mov (:movement (rejected ws)))
                  [tp tn fp fn] (tp-tn-fp-fn true-movs acc-movs rej-movs)]
              (calc-prec-recall tp tn fp fn (count true-movs))))]
      (merge (last metrics)
             {:AvgPrec (avg (map :Prec metrics))
              :AvgRecall (avg (map :Recall metrics))
              :AvgF1 (avg (map :F1 metrics))
              :AvgTPR (avg (map :TPR metrics))
              :AvgFPR (avg (map :FPR metrics))}))
    {:TP 0 :TN 0 :FP 0 :FN 0 :TPR 0.0 :FPR 0.0 :F1 0.0 :Recall 0.0 :Prec 0.0
     :AvgPrec 0.0 :AvgRecall 0.0 :AvgF1 0.0}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :Recall :Prec
                   :AvgTPR :AvgFPR :AvgPrec :AvgRecall :AvgF1])))

(defn training-stats
  [workspace false-accepted unexplained truedata time-now cycle])
