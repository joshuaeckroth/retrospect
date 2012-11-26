(ns retrospect.evaluate)

(defn calc-increase
  [control-results comparison-results field]
  (let [increase-field (keyword (format "Diff%s" (name field)))
        increase-val (- (or (comparison-results field) 0.0)
                        (or (control-results field) 0.0))]
    {increase-field increase-val}))

(defn avg
  [vals]
  (if (empty? vals) 0.0
      (/ (double (reduce + vals)) (double (count vals)))))

(defn calc-prec-coverage
  [tp tn fp fn event-count]
  (let [coverage (if (= 0 event-count) 1.0
                     (/ (double tp) (double event-count)))
        prec (if (= 0 (+ tp fp)) 1.0 (/ (double tp) (double (+ tp fp))))]
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {:TP tp :TN tn :FP fp :FN fn
     :TPR (if (= 0 (+ tp fn)) 1.0 (/ (double tp) (double (+ tp fn))))
     :FPR (if (= 0 (+ fp tn)) 1.0 (/ (double fp) (double (+ fp tn))))
     :Coverage coverage
     :Prec prec
     :F1 (if (= 0 (+ prec coverage)) 0.0
             (/ (* 2.0 prec coverage) (+ prec coverage)))}))
