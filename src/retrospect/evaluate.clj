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