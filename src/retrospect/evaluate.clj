(ns retrospect.evaluate
  (:use [retrospect.epistemicstates :only [current-ep-state previous-ep-state]]))

(defn add-to-prior
  [results key val]
  (let [c (count results)]
    (if (= c 0) val
        (+ (key (last results)) val))))

(defn calc-increase
  [control comparison k]
  (if (= 0 (k control)) 0.0
      (double (* 100.0 (/ (- (k comparison) (k control)) (k control))))))

(defn calc-ratio
  [control comparison k]
  (if (= 0 (k control)) 0.0
      (double (/ (k comparison) (k control)))))

(defn calc-ratio-increase
  [control-results comparison-results field]
  (let [ratio-field (keyword (format "Ratio%s" (name field)))
        increase-field (keyword (format "Increase%s" (name field)))]
    {ratio-field (calc-ratio control-results comparison-results field)
     increase-field (calc-increase control-results comparison-results field)}))

(defn evaluate
  [problem truedata or-state params]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        prev-ep (previous-ep-state (:ep-state-tree or-state))
        final-log (:final (:log (:workspace prev-ep)))
        results (:results or-state)
        resources (:resources or-state)]
    (update-in
     or-state [:results] conj
     (merge {:Problem (:name problem)}
            params
            ((:evaluate-fn problem) ep-state results (:sensors or-state) truedata params)
            {:Step (:time ep-state)
             :MetaActivations (:meta-activations resources)
             :Milliseconds (add-to-prior results :Milliseconds (:milliseconds resources)) 
             :Unexplained (add-to-prior results :Unexplained (count (:unexplained final-log)))
             :SharedExplains (add-to-prior results :SharedExplains
                                           (count (:shared-explains final-log)))
             :ExplainCycles (:explain-cycles resources)
             :HypothesisCount (:hypothesis-count resources)
             :Compute (:compute resources)
             :Memory (:memory resources)}))))

(defn prefix-params
  [prefix params]
  (zipmap (map (fn [k] (keyword (format "%s%s" prefix (name k)))) (keys params))
          (vals params)))

(defn evaluate-comparative
  [problem control-results comparison-results control-params comparison-params]
  (apply merge
         {:Problem (:name problem)}
         (prefix-params "Control" control-params)
         (prefix-params "Comparison" comparison-params)
         ((:evaluate-comparative-fn problem) control-results comparison-results
          control-params comparison-params)
         (map #(calc-ratio-increase control-results comparison-results %)
              [:MetaActivations :Milliseconds :SharedExplains :Unexplained
               :ExplainCycles :HypothesisCount :Compute :Memory])))
