(ns retrospect.evaluate)

(defn calc-increase
  [control-results comparison-results field]
  (let [increase-field (keyword (format "Diff%s" (name field)))
        increase-val (- (or (comparison-results field) 0.0)
                        (or (control-results field) 0.0))]
    {increase-field increase-val}))

(defn nan-max
  [vals]
  (let [vs (filter #(not (.isNaN %)) (map double vals))]
    (if (empty? vs) Double/NaN
                    (apply max vs))))

(defn nan-min
  [vals]
  (let [vs (filter #(not (.isNaN %)) (map double vals))]
    (if (empty? vs) Double/NaN
        (apply min vs))))

(defn normalize
  [vals]
  (let [vs (filter #(not (.isNaN %)) (map double vals))
        sum (double (reduce + vs))]
    (map (fn [v] (/ v sum)) vs)))

(defn avg
  [vals]
  (let [vs (filter #(not (.isNaN %)) (map double (filter identity vals)))]
    (if (empty? vs) 0.0
        (/ (double (reduce + vs)) (double (count vs))))))

(defn calc-prec-recall
  [tp tn fp fn event-count]
  (let [recall (if (= 0 event-count) 0.0 (/ (double tp) (double event-count)))
        prec (if (= 0 (+ tp fp)) 0.0 (/ (double tp) (double (+ tp fp))))]
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {:TP tp :TN tn :FP fp :FN fn
     :TPR (/ (double tp) (double (+ tp fn)))
     :FPR (/ (double fp) (double (+ fp tn)))
     :TNR (/ (double tn) (double (+ fp tn)))
     :PPV (/ (double tp) (double (+ tp fp)))
     :NPV (/ (double tn) (double (+ tn fn)))
     :FDR (/ (double fp) (double (+ fp tp)))
     :Accuracy (if (= 0 (+ tp tn fp fn)) 0.0 (/ (double (+ tp tn)) (double (+ tp tn fp fn))))
     :Recall recall
     :Prec prec
     :F1 (if (= 0.0 (+ prec recall)) 0.0 (/ (* 2.0 prec recall) (+ prec recall)))}))
