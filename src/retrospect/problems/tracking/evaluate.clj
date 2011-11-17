(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.problems.tracking.movements :only [moves-match?]])
  (:use [retrospect.problems.tracking.hypotheses :only [dets-match?]]))

(defn count-matches
  [true-movements movs]
  (count (filter (fn [m] (some #(moves-match? m %) true-movements)) movs)))

(defn percent-events-correct-wrong
  [true-movements believed-movements]
  (if (empty? true-movements) [100.0 0.0] 
      (let [correct (count-matches true-movements believed-movements)]
        [(double (* 100.0 (/ correct (count true-movements))))
         (double (* 100.0 (/ (- (count believed-movements) correct)
                             (count true-movements))))])))

(defn precision-recall
  [true-movements believed-movements disbelieved-movements]
  (if (empty? true-movements) [1.0 1.0 1.0 1.0]
      (let [true-pos (count-matches true-movements believed-movements)
            false-pos (- (count believed-movements) true-pos)
            false-neg (count-matches true-movements disbelieved-movements)
            true-neg (- (count disbelieved-movements) false-neg)]
        ;; precision
        [(if (= 0 (+ true-pos false-pos)) 1.0
             (double (/ true-pos (+ true-pos false-pos)))) 
         ;; recall
         (if (= 0 (+ true-pos false-neg)) 1.0
             (double (/ true-pos (+ true-pos false-neg)))) 
         ;; specificity
         (if (= 0 (+ true-neg false-pos)) 1.0
             (double (/ true-neg (+ true-neg false-pos)))) 
         ;; accuracy
         (if (= 0 (+ true-neg true-pos false-neg false-pos)) 1.0
             (double (/ (+ true-pos true-neg)
                        (+ true-neg true-pos false-neg false-pos))))])))

(defn id-correct
  [true-movements entities maxtime]
  (double (/ (count (filter (fn [e] (dets-match? (get entities e)
                                                 (nth (get true-movements e) maxtime)))
                            (keys true-movements)))
             (count true-movements))))

(defn evaluate
  [ep-state sensors true-movements]
  (let [maxtime (dec (:time ep-state))
        pdata (:problem-data ep-state)
        believed-movements (:believed-movements pdata)
        disbelieved-movements (:disbelieved-movements pdata)
        true-movs (filter #(<= (:time %) maxtime)
                          (apply concat (vals true-movements)))
        [pec pew] (percent-events-correct-wrong true-movs believed-movements)
        [p r s a] (precision-recall true-movs believed-movements disbelieved-movements)]
    {:PEC pec
     :PEW pew
     :Prec p
     :Recall r
     :Spec s
     :Acc a
     :IDCorrect (id-correct true-movements (:entities pdata) maxtime)}))

(defn evaluate-comparative
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:PEC :PEW :Prec :Recall :Spec :Acc :IDCorrect])))
