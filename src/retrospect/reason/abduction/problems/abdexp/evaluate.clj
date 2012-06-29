(ns retrospect.reason.abduction.problems.abdexp.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state]))

(defn true-vertices-now
  [truedata time-now]
  (let [true-vertices (:true-vertices truedata)
        expgraph (get (:test truedata) time-now)]
    (set/intersection true-vertices (vertices expgraph))))

(defn true-hyp?
  [truedata time-now hyp]
  (or (not= :expl (:type hyp))
      (if ((true-vertices-now truedata time-now) (:vertex hyp)) true false)))

(defn count-matches
  [true-vertices vertices]
  (count (filter true-vertices vertices)))

(defn tp-tn-fp-fn
  [true-vertices acc-vertices not-acc-vertices]
  (if (empty? true-vertices) [1.0 1.0 1.0 1.0]
      (let [true-pos (count-matches true-vertices acc-vertices)
            false-pos (- (count acc-vertices) true-pos)
            false-neg (count-matches true-vertices not-acc-vertices)
            true-neg (- (count not-acc-vertices) false-neg)]
        [true-pos true-neg false-pos false-neg])))

(defn evaluate
  [truedata est]
  (let [time-now (:time (cur-ep est))
        ws (:workspace (cur-ep est))
        true-vertices-now (true-vertices-now truedata time-now)
        acc-vertices (set (map #(:vertex (lookup-hyp ws %))
                             (get (:accepted ws) :expl)))
        not-acc-vertices (set/difference (set (map #(:vertex (lookup-hyp ws %))
                                                 (get (:hypotheses ws) :expl)))
                                         acc-vertices)
        [tp tn fp fn] (tp-tn-fp-fn true-vertices-now acc-vertices not-acc-vertices)]
    (println {:TP tp :TN tn :FP fp :FN fn
              :TPR (if (= 0 (+ tp fn)) 1.0 (/ (double tp) (double (+ tp fn))))
              :FPR (if (= 0 (+ fp tn)) 1.0 (/ (double fp) (double (+ fp tn))))
              :F1 (if (= 0 (+ tp fp fn)) 1.0 (/ (double (* 2.0 tp))
                                                (double (+ (* 2.0 tp) fp fn))))})
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {:TP tp :TN tn :FP fp :FN fn
     :TPR (if (= 0 (+ tp fn)) 1.0 (/ (double tp) (double (+ tp fn))))
     :FPR (if (= 0 (+ fp tn)) 1.0 (/ (double fp) (double (+ fp tn))))
     :F1 (if (= 0 (+ tp fp fn)) 1.0 (/ (double (* 2.0 tp))
                                       (double (+ (* 2.0 tp) fp fn))))}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1])))

(defn stats
  [truedata ors time-now])
