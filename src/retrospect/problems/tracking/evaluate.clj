(ns retrospect.problems.tracking.evaluate
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.colors :only [match-color?]])
  (:use [retrospect.problems.tracking.movements :only [moves-match? dets-match?]]))

(defn true-hyp?
  [truedata pdata time hyp]
  (let [true-moves (apply concat (vals truedata))]
    (cond (= :movement (:type hyp))
          (some #(moves-match? (:movement (:data hyp)) %) true-moves)
          (= :path (:type hyp))
          (every? (fn [m] (some #(moves-match? m %) true-moves))
                  (map (comp :movement :data) (:movements (:data hyp))))
          (= :location (:type hyp))
          (dets-match? (assoc (:loc (:data hyp)) :color (:color (:data hyp)))
                       (nth (get truedata (:entity (:data hyp))) time))
          :else true)))

(defn hyps-equal?
  [hyp1 hyp2]
  (let [d1 (:data hyp1)
        d2 (:data hyp2)]
    (if (not= (:type hyp1) (:type hyp2)) false
        (cond (= :movement (:type hyp1))
              (= (:movement d1) (:movement d2))
              (= :path (:type hyp1))
              (and (= (:movements d1) (:movements d2))
                   (= (:bias d1) (:bias d2)))
              (= :location (:type hyp1))
              (and (= (:entity d1) (:entity d2))
                   (= (:bias d1) (:bias d2))
                   (match-color? (:color d1) (:color d2))
                   (= (:loc d1) (:loc d2)))
              :else false))))

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

(defn id-correct
  [true-movements entities maxtime]
  (double (/ (count (filter (fn [e] (dets-match? (last (get entities e))
                                                 (nth (get true-movements e) maxtime)))
                            (keys true-movements)))
             (count true-movements))))

(defn evaluate
  [ep-state sensors truedata]
  (let [maxtime (:time ep-state)
        pdata (:problem-data ep-state)
        believed-movements (:believed-movements pdata)
        disbelieved-movements (:disbelieved-movements pdata)
        true-movs (filter #(and (:ot %) (<= (:time %) maxtime))
                          (apply concat (vals truedata)))
        [pec pew] (percent-events-correct-wrong true-movs believed-movements)
        [tp tn fp fn p r s a] (precision-recall true-movs believed-movements disbelieved-movements)]
    {:PEC pec
     :PEW pew
     :Prec p
     :Recall r
     :Spec s
     :Acc a
     :TruePositive tp
     :TrueNegative tn
     :FalsePositive fp
     :FalseNegative fn
     :IDCorrect (id-correct truedata (:entities pdata) maxtime)}))

(defn evaluate-comparative
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:PEC :PEW :Prec :Recall :Spec :Acc
                     :TruePositive :TrueNegative :FalsePositive :FalseNegative
                     :IDCorrect])))
