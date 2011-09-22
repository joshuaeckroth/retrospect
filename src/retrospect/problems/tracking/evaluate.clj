(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.evaluate :only [calc-ratio-increase]])
  (:use [retrospect.problems.tracking.hypotheses :only [paths-to-movements]])
  (:use [retrospect.problems.tracking.truedata :only [true-movements]])
  (:require [clojure.set :as set]))

(defn percent-events-correct-wrong
  [pdata true-moves]
  (if (empty? true-moves) [100.0 0.0] 
    (let [moves (set (paths-to-movements (:paths pdata)))
          correct (set/intersection true-moves moves)]
      [(double (* 100.0 (/ (count correct) (count true-moves))))
       (double (* 100.0 (/ (- (count moves) (count correct)) (count true-moves))))])))

(defn precision-recall
  [pdata true-moves]
  (if (empty? true-moves) [1.0 1.0 1.0 1.0]
    (let [believed-moves (set (paths-to-movements (:paths pdata)))
          disbelieved-moves (set (:disbelieved-moves pdata))
          true-pos (count (set/intersection true-moves believed-moves))
          false-pos (- (count believed-moves) true-pos)
          false-neg (count (set/intersection true-moves disbelieved-moves))
          true-neg (- (count disbelieved-moves) false-neg)]
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
  [ep-state results sensors truedata params]
  (let [maxtime (min (dec (dec (count truedata))) (dec (dec (:time ep-state))))
        true-moves (true-movements truedata maxtime)
        [pec pew] (percent-events-correct-wrong (:problem-data ep-state) true-moves)
        [p r s a] (precision-recall (:problem-data ep-state) true-moves)]
    {:PEC pec
     :PEW pew
     :Precision p
     :Recall r
     :Specificity s
     :Accuracy a}))

(defn evaluate-comparative
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-ratio-increase control-results comparison-results %)
                    [:PEC :PEW :Precision :Recall :Specificity :Accuracy])))
