(ns retrospect.evaluate
  (:use [retrospect.epistemicstates :only [current-ep-state previous-ep-state]])
  (:use [retrospect.state]))

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
  (let [ratio-field (keyword (format "Rat%s" (name field)))
        increase-field (keyword (format "Inc%s" (name field)))]
    {ratio-field (calc-ratio control-results comparison-results field)
     increase-field (calc-increase control-results comparison-results field)}))

(defn evaluate
  [truedata or-state]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        prev-ep (previous-ep-state (:ep-state-tree or-state))
        final-log (:final (:log (:workspace prev-ep)))
        results (:results or-state)
        resources (:resources or-state)]
    (update-in
     or-state [:results] conj
     (merge {:Problem (:name @problem)}
            params
            ((:evaluate-fn @problem) ep-state results (:sensors or-state) truedata)
            {:Step (:time ep-state)
             :MetaActivations (:meta-activations resources)
             :MetaAccepted (:meta-accepted resources)
             :Milliseconds (add-to-prior results :Milliseconds (:milliseconds resources)) 
             :Unexplained (add-to-prior results :Unexplained (count (:unexplained final-log)))
             :NoExplainers (add-to-prior results :NoExplainers (count (:no-explainers final-log)))
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
  [control-results comparison-results control-params comparison-params]
  (apply merge
         {:Problem (:name @problem)}
         (prefix-params "Cont" control-params)
         (prefix-params "Comp" comparison-params)
         ((:evaluate-comparative-fn @problem) control-results comparison-results
          control-params comparison-params)
         (map #(calc-ratio-increase control-results comparison-results %)
              [:MetaActivations :MetaAccepted :Milliseconds :SharedExplains
               :Unexplained :NoExplainers :ExplainCycles :HypothesisCount :Compute :Memory])))
