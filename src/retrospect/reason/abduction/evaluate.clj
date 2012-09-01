(ns retrospect.reason.abduction.evaluate
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct calc-doubt calc-coverage
                accepted? lookup-hyp]])
  (:use [retrospect.state]))

(defn group-hyps-by-true-false
  [hyps type-key truedata time true-hyp?]
  (let [hs (group-by type-key hyps)
        tf (reduce (fn [m type]
                (let [grouped (group-by (fn [h] (if (true-hyp? truedata time h)
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
  [workspace true-false]
  ;; TODO: fix
  (comment
    (let [delta-tf (let [b (:best (:log workspace))]
                     [(get-in true-false [:individual (:id (:best b))])
                      (:delta b)])
          delta-true (map second (filter first delta-tf))
          delta-false (map second (filter (comp not first) delta-tf))]
      {:true-delta-avg (/ (reduce + delta-true) (double (let [c (count delta-true)]
                                                     (if (= 0 c) 1 c))))
       :false-delta-avg (/ (reduce + delta-false) (double (let [c (count delta-false)]
                                                       (if (= 0 c) 1 c))))}))
  {:true-delta-avg 0.0
   :false-delta-avg 0.0})

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        eps (flatten-est est)
        workspace (:workspace ep)
        true-false (group-hyps-by-true-false (vals (:hyp-ids workspace))
                    :type truedata (:time ep) (:true-hyp?-fn (:abduction @problem)))
        true-false-scores (calc-true-false-scores workspace true-false)
        delta-avgs (calc-true-false-deltas workspace true-false)
        ep-states (flatten-est est)]
    (merge {:Problem (:name @problem)}
           params
           ((:evaluate-fn (:abduction @problem)) truedata est)
           true-false-scores
           {:Step (:time ep)
            :UnexplainedPct (get-unexp-pct (:workspace ep))
            :NoExplainersPct (get-noexp-pct (:workspace ep))
            :TrueDeltaAvg (:true-delta-avg delta-avgs)
            :FalseDeltaAvg (:false-delta-avg delta-avgs)
            :Doubt (calc-doubt (:workspace ep))
            :Coverage (calc-coverage (:workspace ep))
            :ExplainCycles (count ep-states)
            :MetaBranches (count (filter #(second (:children %)) ep-states))
            :HypothesisCount ((comp count :hyp-ids :workspace) ep)})))

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
                               :MetaBranches]
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
