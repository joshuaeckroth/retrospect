(ns retrospect.reason.abduction.problems.tracking.evaluate
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.problems.tracking.colors :only [match-color?]])
  (:use [retrospect.problems.tracking.movements :only [moves-match? dets-match?]]))

(defn true-hyp?
  [truedata time hyp]
  (let [true-movs (apply concat (vals (:test truedata)))]
    (cond (= :movement (:type hyp))
          (some #(moves-match? (:mov hyp) %) true-movs)
          (= :path (:type hyp))
          (every? (fn [m] (some #(moves-match? m %) true-movs))
                  (map :mov (:movs hyp)))
          (= :location (:type hyp))
          (dets-match? (assoc (:loc hyp) :color (:color hyp))
                       (nth (get truedata (:entity hyp)) time))
          :else true)))

(defn hyps-equal?
  [hyp1 hyp2]
  (if (not= (:type hyp1) (:type hyp2)) false
      (cond (= :movement (:type hyp1))
            (= (:mov hyp1) (:mov hyp2))
            (= :path (:type hyp1))
            (and (= (:movs hyp1) (:movs hyp2)))
            (= :location (:type hyp1))
            (and (= (:entity hyp1) (:entity hyp2))
                 (= (:bias hyp1) (:bias hyp2))
                 (match-color? (:color hyp1) (:color hyp2))
                 (= (:loc hyp1) (:loc hyp2)))
            :else false)))

(defn count-matches
  [true-movs movs]
  (count (filter (fn [m] (some #(moves-match? m %) true-movs)) movs)))

(defn percent-events-correct-wrong
  [true-movs bel-movs]
  (if (empty? true-movs) [1.0 0.0] 
      (let [correct (count-matches true-movs bel-movs)]
        [(double (/ correct (count true-movs)))
         (double (/ (- (count bel-movs) correct)
                    (count true-movs)))])))

(defn precision-recall
  [true-movs bel-movs disbel-movs]
  (if (empty? true-movs) [1.0 1.0 1.0 1.0]
      (let [true-pos (count-matches true-movs bel-movs)
            false-pos (- (count bel-movs) true-pos)
            false-neg (count-matches true-movs disbel-movs)
            true-neg (- (count disbel-movs) false-neg)]
        
        [true-pos true-neg false-pos false-neg
         ;; precision
         (if (= 0 (+ true-pos false-pos)) 1.0
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

(defn evaluate
  [truedata est]
  (let [eps (rest (flatten-est est))
        time-now (:time (last eps))
        true-movs (filter #(and (:ot %) (<= (:time %) time-now))
                          (apply concat (vals (:test truedata))))
        accepted (:accepted (:workspace (last eps)))
        rejected (:rejected (:workspace (last eps)))
        bel-movs (map :mov (get accepted :movement))
        disbel-movs (map :mov (get rejected :movement))
        [pec pew] (percent-events-correct-wrong true-movs bel-movs)
        [tp tn fp fn p r s a] (precision-recall true-movs bel-movs disbel-movs)]
    {:PEC pec
     :PEW pew
     :Prec p
     :Recall r
     :Spec s
     :Acc a
     :TP tp
     :TN tn
     :FP fp
     :FN fn}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:PEC :PEW :Prec :Recall :Spec :Acc
                     :TP :TN :FP :FN
                     :ID])))
