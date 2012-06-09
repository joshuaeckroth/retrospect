(ns retrospect.reason.abduction.problems.tracking.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp calc-doubt]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata _ hyp]
  (if (= :movement (:type hyp))
    ((:all-moves truedata) (:mov hyp))
    true))

(defn hyps-equal?
  [hyp1 hyp2]
  (if (not= (:type hyp1) (:type hyp2)) false
      (cond (= :movement (:type hyp1))
            (= (:mov hyp1) (:mov hyp2))
            :else false)))

(defn count-matches
  [true-movs movs]
  (count (filter true-movs movs)))

(defn tp-tn-fp-fn
  [true-movs acc-movs not-acc-movs]
  (if (empty? true-movs) [1.0 1.0 1.0 1.0]
      (let [true-pos (count-matches true-movs acc-movs)
            false-pos (- (count acc-movs) true-pos)
            false-neg (count-matches true-movs not-acc-movs)
            true-neg (- (count not-acc-movs) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn get-true-movements
  [truedata time-now]
  (set (filter #(and (:ot %) (<= (:time %) time-now))
          (:all-moves truedata))))

(defn evaluate-helper
  [truedata workspace time-now]
  (let [true-movs (get-true-movements truedata time-now)
        accepted (map #(lookup-hyp workspace %)
                    (:movement (:accepted workspace)))
        not-accepted (set/difference
                      (set (map #(lookup-hyp workspace %)
                              (:movement (:hypotheses workspace))))
                      accepted)
        acc-movs (map :mov accepted)
        not-acc-movs (map :mov not-accepted)
        [tp tn fp fn] (tp-tn-fp-fn true-movs acc-movs not-acc-movs)]
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {:TP tp :TN tn :FP fp :FN fn
     :TPR (if (= 0 (+ tp fn)) 1.0 (/ (double tp) (double (+ tp fn))))
     :FPR (if (= 0 (+ fp tn)) 1.0 (/ (double fp) (double (+ fp tn))))
     :F1 (if (= 0 (+ tp fp fn)) 1.0 (/ (double (* 2.0 tp))
                                       (double (+ (* 2.0 tp) fp fn))))}))

(defn evaluate
  [truedata est]
  (prof :evaluate
        (let [eps (rest (flatten-est est))
              ws (:workspace (last eps))
              time-now (:time (last eps))]
          (evaluate-helper truedata ws time-now))))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:TP :TN :FP :FN :TPR :FPR :F1])))

(defn training-stats
  [workspace false-accepted unexplained truedata time-now temp]
  (when (or (>= 0.0 temp) (= (:StartingTemp params) temp)
            (and (= 0 (count false-accepted))
                 (= 0 (count unexplained))))
    (when (and (= 1 time-now) (= (:StartingTemp params) temp))
      (.print System/out "time,pctfalseacc,doubt,maxadjlength,minadjustlength,avgadjustlength,avgmaxadjust,avgminadjust,numadjust,tpr,fpr,f1,begend\n"))
    (let [adjustments (vals (:score-adjustments workspace))
          max-adjust-length (if (empty? adjustments) 0 (apply max (map count adjustments)))
          min-adjust-length (if (empty? adjustments) 0 (apply min (map count adjustments)))
          avg-adjust-length (/ (double (reduce + (map count adjustments))) (double (count adjustments)))
          avg-max-adjusted-score (/ (double (reduce + (map #(apply max 0.0 %) adjustments)))
                                    (double (count adjustments)))
          avg-min-adjusted-score (/ (double (reduce + (map #(apply min 1.0 %) adjustments)))
                                    (double (count adjustments)))
          {:keys [TPR FPR F1]} (evaluate-helper truedata workspace time-now)]
      (.print System/out (format "%d,%.4f,%.4f,%d,%d,%.2f,%.2f,%.2f,%d,%.4f,%.4f,%.4f,\"%s\"\n"
                            time-now
                            (double (/ (count false-accepted)
                                       (count (:forced workspace))))
                            (calc-doubt workspace)
                            max-adjust-length min-adjust-length avg-adjust-length
                            avg-max-adjusted-score avg-min-adjusted-score
                            (count adjustments) TPR FPR F1
                            (if (= (:StartingTemp params) temp) "beg" "end"))))))
