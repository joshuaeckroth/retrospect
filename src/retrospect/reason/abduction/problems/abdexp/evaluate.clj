(ns retrospect.reason.abduction.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata time-now hyp]
  (if (= :kb (:type hyp)) true
      (if ((:true-explainers truedata) (:vertex hyp)) true false)))

(defn count-matches
  [true-vertices vertices]
  (count (filter true-vertices vertices)))

(defn tp-tn-fp-fn
  [true-vertices acc-vertices not-acc-vertices]
  (if (empty? true-vertices) [0 0 0 0]
      (let [true-pos (count-matches true-vertices acc-vertices)
            false-pos (- (count acc-vertices) true-pos)
            false-neg (count-matches true-vertices not-acc-vertices)
            true-neg (- (count not-acc-vertices) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn evaluate
  [truedata est]
  (let [time-now (:time (cur-ep est))
        ws (:workspace (cur-ep est))
        acc-explainers (set (map #(:vertex (lookup-hyp ws %)) (get (:accepted ws) :expl)))
        not-acc-explainers (set/difference (set (map #(:vertex (lookup-hyp ws %))
                                                   (get (:hypotheses ws) :expl)))
                                           acc-explainers)
        [tp tn fp fn] (tp-tn-fp-fn (:true-explainers truedata)
                                   acc-explainers not-acc-explainers)]
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {:TP tp :TN tn :FP fp :FN fn
     :TPR (if (= 0 (+ tp fn)) 1.0 (/ (double tp) (double (+ tp fn))))
     :FPR (if (= 0 (+ fp tn)) 1.0 (/ (double fp) (double (+ fp tn))))
     :F1 (if (= 0 (+ tp fp fn)) 1.0 (/ (double (* 2.0 tp))
                                       (double (+ (* 2.0 tp) fp fn))))
     :TPRatio (if (empty? (:true-explainers truedata)) 1.0
                  (/ (double tp) (double (count (:true-explainers truedata)))))
     :Prec (if (= 0 (+ tp fp)) 1.0 (/ (double tp) (double (+ tp fp))))}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :TPRatio :Prec])))

(defn stats
  [truedata ors time-now])
