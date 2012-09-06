(ns retrospect.reason.abduction.evaluate
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [ep-path]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct calc-doubt calc-coverage
                accepted? rejected? rejection-reason lookup-hyp update-graph
                accepted-explained get-no-explainers explainers]])
  (:use [retrospect.state]))

(defn doubt-aggregate
  [est]
  (let [doubts (filter identity (map #(calc-doubt %) (map :workspace (ep-path est))))]
    (cond (= "avg" (:DoubtAggregate params))
          (if (empty? doubts) 0.0 (/ (reduce + doubts) (double (count doubts))))
          (= "max" (:DoubtAggregate params))
          (if (empty? doubts) 0.0 (apply max doubts))
          (= "min" (:DoubtAggregate params))
          (if (empty? doubts) 0.0 (apply min doubts)))))

(defn group-hyps-by-true-false
  [hyps type-key truedata true-hyp?]
  (let [hs (group-by type-key hyps)
        tf (reduce (fn [m type]
                (let [grouped (group-by (fn [h] (if (true-hyp? truedata h)
                                                 true false))
                                        (get hs type))
                      m-individual (reduce (fn [m2 [tf hs]]
                                        (reduce (fn [m3 h] (assoc-in m3 [:individual (:id h)] tf))
                                           m2 hs))
                                      m (seq grouped))]
                  (assoc m-individual type
                         (reduce (fn [g tf] (if (nil? (get g tf)) (assoc g tf []) g))
                            grouped [true false]))))
              {} (set (concat (keys hs) (:hyp-types (:abduction @problem)))))
        all-true (mapcat #(get % true) (vals tf))
        all-false (mapcat #(get % false) (vals tf))]
    (assoc tf :all {true all-true false all-false})))

(defn calc-true-false-scores
  "Find average scores for true hyps, average scores for false hyps."
  [workspace true-false]
  (let [aprioris (reduce (fn [m t]
                      (assoc m t
                             {true (map :apriori (get (get true-false t) true))
                              false (map :apriori (get (get true-false t) false))}))
                    {} (keys true-false))
        avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + vals) (count vals))))]
    (reduce (fn [m t]
         (let [k (apply str (map str/capitalize (str/split (name t) #"-")))]
           (assoc m
             (keyword (format "TrueCount%s" k))
             (count (get (get true-false t) true))
             (keyword (format "FalseCount%s" k))
             (count (get (get true-false t) false))
             (keyword (format "TrueAcc%s" k))
             (count (filter #(accepted? workspace %) (get (get true-false t) true)))
             (keyword (format "FalseAcc%s" k))
             (count (filter #(accepted? workspace %) (get (get true-false t) false)))
             (keyword (format "AvgTrueApriori%s" k))
             (avg (get (get aprioris t) true))
             (keyword (format "AvgFalseApriori%s" k))
             (avg (get (get aprioris t) false)))))
       {} (keys true-false))))

(defn calc-true-false-deltas
  "Find average delta for true and false acceptances."
  [est true-false]
  (let [delta-tf (for [ep (filter (comp :best :accrej :workspace) (ep-path est))]
                   (let [accrej (:accrej (:workspace ep))]
                     [(get-in true-false [:individual (:id (:best accrej))])
                      (:delta accrej)]))
        delta-true (map second (filter first delta-tf))
        delta-false (map second (filter (comp not first) delta-tf))]
    {:true-delta-avg (/ (reduce + delta-true) (double (let [c (count delta-true)]
                                                   (if (= 0 c) 1 c))))
     :false-delta-avg (/ (reduce + delta-false) (double (let [c (count delta-false)]
                                                     (if (= 0 c) 1 c))))}))

(defn find-errors
  [est true-false]
  ;; only need to look at last workspace; it contains all the history
  (let [ws (:workspace (cur-ep est))]
    (frequencies
     (for [hyp (filter #(not= :kb (:type %)) (vals (:hyp-ids ws)))]
       (cond
        ;; obs that's false yet was accepted; should have been ignored/rejected
        (and (= :observation (:type hyp))
             (accepted? ws hyp)
             (not (get-in true-false [:individual (:id hyp)])))
        :noise
        ;; conflict-rejection: a true thing was rejected as a conflict
        ;; (after accepting a false thing, necessarily)
        (and (rejected? ws hyp)
             (= :conflict (rejection-reason ws hyp))
             (get-in true-false [:individual (:id hyp)]))
        :conflict-rejection
        ;; true thing eliminated due to too-low minapriori
        (and (rejected? ws hyp)
             (= :minapriori (rejection-reason ws hyp))
             (get-in true-false [:individual (:id hyp)]))
        :minapriori
        ;; scoring error: if you were accepted but are false, and the
        ;; thing you explained is true; or you were not accepted but
        ;; were true, and the thing you explained is true (this makes
        ;; 2 errors instead of 1 for the same 'acceptance')
        (or (and (accepted? ws hyp)
                 (not (get-in true-false [:individual (:id hyp)]))
                 (get-in true-false [:individual (:id (accepted-explained ws hyp))]))
            (and (not (accepted? ws hyp))
                 (get-in true-false [:individual (:id hyp)])
                 (get-in true-false [:individual (:id (accepted-explained ws hyp))])))
        :scoring
        ;; a false thing was accepted, or true thing not accepted (and
        ;; threshold = 0); must be an order-dependency error if none
        ;; of the above errors are the cause
        (or (and (accepted? ws hyp)
                 (not (get-in true-false [:individual (:id hyp)])))
            (and (not (accepted? ws hyp))
                 (get-in true-false [:individual (:id hyp)])
                 (= 0 (:Threshold params))))
        :unknown
        ;; else, there was no error
        :else
        :no-error)))))

;; true reasons why something cannot be explained (a noexp requires it
;; was accepted): noise, true explainer was rejected due to conflict;
;; true explainer was rejected due to minapriori
(defn find-noexp-reasons
  [est true-false]
  (let [ws (:workspace (cur-ep est))
        noexp (map #(lookup-hyp ws %) (get-no-explainers ws))]
    (frequencies
     (for [hyp noexp]
       (let [true-expl (filter #(get-in true-false [:individual (:id %)])
                          (explainers ws hyp))]
         (cond (and (= :observation (:type hyp))
                    (not (get-in true-false [:individual (:id hyp)])))
               :noise
               (some #(and (rejected? ws %) (= :conflict (rejection-reason ws %))) true-expl)
               :expl-rejected-conflict
               (some #(and (rejected? ws %) (= :minapriori (rejection-reason ws %))) true-expl)
               :expl-rejected-minapriori
               (empty? true-expl)
               :no-expl-offered
               :else
               :unknown))))))

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        eps (flatten-est est)
        workspace (update-graph (:workspace ep))
        true-false (group-hyps-by-true-false
                    (vals (:hyp-ids workspace))
                    :type truedata (:oracle-fn @problem))
        true-false-scores (calc-true-false-scores workspace true-false)
        delta-avgs (calc-true-false-deltas est true-false)
        ep-states (flatten-est est)
        doubt (doubt-aggregate est)
        errors (find-errors est true-false)
        noexp-reasons (find-noexp-reasons est true-false)]
    (merge {:Problem (:name @problem)}
           params
           ((:evaluate-fn (:abduction @problem)) truedata est)
           true-false-scores
           {:Step (:time ep)
            :UnexplainedPct (get-unexp-pct (:workspace ep))
            :NoExplainersPct (get-noexp-pct (:workspace ep))
            :TrueDeltaAvg (:true-delta-avg delta-avgs)
            :FalseDeltaAvg (:false-delta-avg delta-avgs)
            :Doubt (doubt-aggregate est)
            :Coverage (calc-coverage workspace)
            :ExplainCycles (count ep-states)
            :MetaBranches (count (filter #(second (:children %)) ep-states))
            :HypothesisCount ((comp count :hyp-ids :workspace) ep)
            :ErrorsCount (reduce + (vals (dissoc errors :no-error)))
            :ErrorsNoise (:noise errors 0)
            :ErrorsConflictRejection (:conflict-rejection errors 0)
            :ErrorsMinApriori (:minapriori errors 0)
            :ErrorsScoring (:scoring errors 0)
            :ErrorsUnknown (:unknown errors 0)
            :ErrorsNoError (:no-error errors 0)
            :NoExpCount (reduce + (vals noexp-reasons))
            :NoExpReasonNoise (:noise noexp-reasons 0)
            :NoExpReasonRejectedConflict (:expl-rejected-conflict noexp-reasons 0)
            :NoExpReasonRejectedMinApriori (:expl-rejected-minapriori noexp-reasons 0)
            :NoExpReasonNoExpl (:no-expl-offered noexp-reasons 0)
            :NoExpReasonUnknown (:unknown noexp-reasons 0)})))

(defn prefix-params
  [prefix params]
  (zipmap (map (fn [k] (keyword (format "%s%s" prefix (name k)))) (keys params))
          (vals params)))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (letfn [(do-eval [control comparison]
            (apply merge
                   {:Problem (:name @problem)}
                   (prefix-params "Cont" (dissoc control-params :simulation))
                   (prefix-params "Comp" (dissoc comparison-params :simulation))
                   {:simulation (:simulation control-params)
                    :Step (:Step control)}
                   ((:evaluate-comp-fn (:abduction @problem)) control comparison
                    control-params comparison-params)
                   (map #(calc-increase control comparison %)
                      (concat [:UnexplainedPct :NoExplainersPct
                               :TrueDeltaAvg :FalseDeltaAvg
                               :Doubt :Coverage :ExplainCycles :HypothesisCount
                               :MetaBranches :ErrorsCount :ErrorsNoise
                               :ErrorsConflictRejection :ErrorsMinApriori
                               :ErrorsScoring :ErrorsUnknown :ErrorsNoError :NoExpCount
                               :NoExpReasonNoise :NoExpReasonRejectedConflict
                               :NoExpReasonRejectedMinApriori :NoExpReasonNoExpl
                               :NoExpReasonUnknown]
                              (mapcat
                               (fn [tf]
                                 (map #(keyword
                                      (format "%s%s" tf
                                         (apply str
                                                (map str/capitalize
                                                   (str/split (name %) #"-")))))
                                    (:hyp-subtypes @problem)))
                               ["AvgTrueConf" "AvgTrueApriori"
                                "AvgFalseConf" "AvgFalseApriori"
                                "TrueCount" "FalseCount" "TrueAcc" "FalseAcc"])))))]
    ;; if control/comparison have different number of results
    ;; (different steps between or steps), then just use the last
    ;; result set
    (if (not= (count control-results) (count comparison-results))
      (let [control (last control-results)
            comparison (last comparison-results)]
        [(do-eval control comparison)])
      ;; otherwise, evaluate each result set in sequence
      (for [i (range (count control-results))]
        (let [control (nth control-results i)
              comparison (nth comparison-results i)]
          (do-eval control comparison))))))
