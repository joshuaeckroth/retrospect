(ns retrospect.reason.abduction.problems.tracking.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.problems.tracking.colors :only [match-color?]])
  (:use [retrospect.problems.tracking.movements :only [moves-match? dets-match?]])
  (:use [retrospect.profile :only [prof]]))

(defn true-hyp?
  [truedata time hyp]
  (let [true-movs (apply concat (vals (:test truedata)))]
    (cond (= :movement (:type hyp))
          (some #(moves-match? (:mov hyp) %) true-movs)
          :else true)))

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
  (if (empty? true-movs) [1.0 1.0 1.0 1.0]
      (let [true-pos (count-matches true-movs acc-movs)
            false-pos (- (count acc-movs) true-pos)
            false-neg (count-matches true-movs not-acc-movs)
            true-neg (- (count not-acc-movs) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn get-true-movements
  [truedata time-now]
  (filter #(and (:ot %) (<= (:time %) time-now))
     (apply concat (vals (:test truedata)))))

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
