(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.evaluate :only [calc-ratio-increase]])
  (:require [clojure.set :as set]))

(defn percent-events-correct-wrong
  [true-movements believed-movements]
  (if (empty? true-movements) [100.0 0.0] 
      (let [correct (set/intersection true-movements believed-movements)]
        [(double (* 100.0 (/ (count correct) (count true-movements))))
         (double (* 100.0 (/ (- (count believed-movements) (count correct))
                             (count true-movements))))])))

(defn precision-recall
  [true-movements believed-movements disbelieved-movements]
  (if (empty? true-movements) [1.0 1.0 1.0 1.0]
      (let [true-pos (count (set/intersection true-movements believed-movements))
            false-pos (- (count believed-movements) true-pos)
            false-neg (count (set/intersection true-movements disbelieved-movements))
            true-neg (- (count disbelieved-movements) false-neg)]
        ;; precision
        [(if (= 0 (+ true-pos false-pos)) 0.0
             (double (/ true-pos (+ true-pos false-pos)))) 
         ;; recall
         (if (= 0 (+ true-pos false-neg)) 0.0
             (double (/ true-pos (+ true-pos false-neg)))) 
         ;; specificity
         (if (= 0 (+ true-neg false-pos)) 0.0
             (double (/ true-neg (+ true-neg false-pos)))) 
         ;; accuracy
         (if (= 0 (+ true-neg true-pos false-neg false-pos)) 0.0
             (double (/ (+ true-pos true-neg)
                        (+ true-neg true-pos false-neg false-pos))))])))

(defn evaluate
  [ep-state results sensors true-movements]
  (let [maxtime (dec (:time ep-state))
        pdata (:problem-data ep-state)
        believed-movements (:believed-movements pdata)
        disbelieved-movements (:disbelieved-movements pdata)
        true-movs (set (filter #(<= (:time %) maxtime)
                               (apply concat (vals true-movements))))
        [pec pew] (percent-events-correct-wrong true-movs believed-movements)
        [p r s a] (precision-recall true-movs believed-movements disbelieved-movements)]
    {:PEC pec
     :PEW pew
     :Prec p
     :Recall r
     :Spec s
     :Acc a}))

(defn evaluate-comparative
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-ratio-increase control-results comparison-results %)
                    [:PEC :PEW :Prec :Recall :Spec :Acc])))
