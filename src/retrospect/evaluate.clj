(ns retrospect.evaluate)

(defn calc-increase
  [control-results comparison-results field]
  (let [increase-field (keyword (format "Diff%s" (name field)))
        increase-val (- (or (comparison-results field) 0.0)
                        (or (control-results field) 0.0))]
    (println "hello 6.")
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
  [tp tn fp fn event-count prefix]
  (let [recall (if (= 0 event-count) 0.0 (/ (double tp) (double event-count)))
        prec (if (= 0 (+ tp fp)) 0.0 (/ (double tp) (double (+ tp fp))))]
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {(keyword (format "%sTP" prefix)) tp
     (keyword (format "%sTN" prefix)) tn
     (keyword (format "%sFP" prefix)) fp
     (keyword (format "%sFN" prefix)) fn
     (keyword (format "%sTPR" prefix)) (/ (double tp) (double (+ tp fn)))
     (keyword (format "%sFPR" prefix)) (/ (double fp) (double (+ fp tn)))
     (keyword (format "%sTNR" prefix)) (/ (double tn) (double (+ fp tn)))
     (keyword (format "%sPPV" prefix)) (/ (double tp) (double (+ tp fp)))
     (keyword (format "%sNPV" prefix)) (/ (double tn) (double (+ tn fn)))
     (keyword (format "%sFDR" prefix)) (/ (double fp) (double (+ fp tp)))
     (keyword (format "%sAccuracy" prefix)) (if (= 0 (+ tp tn fp fn))
                                              0.0 (/ (double (+ tp tn)) (double (+ tp tn fp fn))))
     (keyword (format "%sRecall" prefix)) recall
     (keyword (format "%sPrec" prefix)) prec
     (keyword (format "%sF1" prefix)) (if (= 0.0 (+ prec recall))
                                        0.0 (/ (* 2.0 prec recall) (+ prec recall)))}))
