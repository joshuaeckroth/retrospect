(ns retrospect.reason.abduction.evaluate
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est count-branches]])
  (:use [retrospect.evaluate :only [calc-increase calc-prec-coverage avg]])
  (:use [retrospect.epistemicstates :only [ep-path]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct calc-doubt calc-coverage
                accepted? rejected? rejection-reason lookup-hyp update-graph
                accepted-before? rejected-before? unexplained?
                accepted-explained accepted-rivals get-no-explainers
                get-unexplained explainers explains find-conflicts-all]])
  (:use [retrospect.state]))

(defn keyword-to-metric
  [kw]
  (apply str (map str/capitalize (str/split (name kw) #"-"))))

(defn doubt-aggregate
  [est]
  (let [doubts (filter identity (map #(calc-doubt %) (map :workspace (ep-path est))))
        unexp (get-unexplained (:workspace (cur-ep est)))
        ds (if (:DoubtUnexp params)
             (concat (repeat (count unexp) 1.0) doubts)
             doubts)]
    (cond (= "avg" (:DoubtAggregate params))
          (if (empty? ds) 0.0 (/ (reduce + ds) (double (count ds))))
          (= "max" (:DoubtAggregate params))
          (if (empty? ds) 0.0 (apply max ds))
          (= "min" (:DoubtAggregate params))
          (if (empty? ds) 0.0 (apply min ds)))))

(defn tf-true?
  [true-false hyp]
  (get-in true-false [:individual (:id hyp)]))

(defn group-hyps-by-true-false
  [hyps type-key truedata true-hyp? meta?]
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
              {} (set (if meta? (:meta-hyp-types @reasoner)
                          (:hyp-types (:abduction @problem)))))
        all-true (mapcat #(get % true) (vals tf))
        all-false (mapcat #(get % false) (vals tf))]
    (assoc tf :all {true all-true false all-false})))

(defn calc-true-false-scores
  "Find average scores for true hyps, average scores for false hyps."
  [est true-false]
  (let [workspace (:workspace (cur-ep est))
        eps (flatten-est est)
        meta-eps (mapcat (comp flatten-est :meta-est) (filter :meta-est eps))
        acc? (fn [h] (if ((set (:meta-hyp-types @reasoner)) (:type h))
                      (some (fn [ep] ((:all (:accepted (:workspace ep))) (:id h)))
                         meta-eps)
                      (accepted? workspace h)))
        aprioris (reduce (fn [m t]
                      (assoc m t
                             {true (map :apriori (get (get true-false t) true))
                              false (map :apriori (get (get true-false t) false))}))
                    {} (keys true-false))
        avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + vals) (count vals))))]
    (reduce (fn [m t]
         (let [k (keyword-to-metric t)]
           (assoc m
             (keyword (format "TrueCount%s" k))
             (count (get (get true-false t) true))
             (keyword (format "FalseCount%s" k))
             (count (get (get true-false t) false))
             (keyword (format "TrueAcc%s" k))
             (count (filter acc? (get (get true-false t) true)))
             (keyword (format "FalseAcc%s" k))
             (count (filter acc? (get (get true-false t) false)))
             (keyword (format "AvgTrueApriori%s" k))
             (avg (get (get aprioris t) true))
             (keyword (format "AvgFalseApriori%s" k))
             (avg (get (get aprioris t) false)))))
       {} (keys true-false))))

(defn calc-true-false-deltas
  "Find average delta for true and false acceptances."
  [est true-false meta?]
  (let [eps (if meta?
              (mapcat (comp flatten-est :meta-est) (filter :meta-est (flatten-est est)))
              (ep-path est))
        delta-tf (for [ep (filter (comp :best :accrej :workspace) eps)]
                   (let [accrej (:accrej (:workspace ep))]
                     [(tf-true? true-false (:best accrej))
                      (:delta accrej)]))
        delta-true (map second (filter first delta-tf))
        delta-false (map second (filter (comp not first) delta-tf))]
    {:true-delta-avg (/ (reduce + delta-true) (double (let [c (count delta-true)]
                                                   (if (= 0 c) 1 c))))
     :false-delta-avg (/ (reduce + delta-false) (double (let [c (count delta-false)]
                                                     (if (= 0 c) 1 c))))}))

(defn classify-error
  ([ws true-false hyp]
     (classify-error ws true-false hyp #{}))
  ([ws true-false hyp checked]
     (cond
      ;; obs that's false yet was accepted, should have been
      ;; ignored/rejected; or a false accepted that explained noise
      (or (and (= :observation (:type hyp))
               (accepted? ws hyp)
               (not (tf-true? true-false hyp)))
          (and (not= :observation (:type hyp))
               (accepted? ws hyp)
               (not (tf-true? true-false hyp))
               (= :observation (:type (accepted-explained ws hyp)))
               (not (tf-true? true-false (accepted-explained ws hyp)))))
      :noise
      ;; this hyp is true and eliminated due to too-low minscore;
      ;; or, this hyp is false and true rival eliminated due to
      ;; too-low minscore
      (or (and (rejected? ws hyp)
               (= :minscore (rejection-reason ws hyp))
               (tf-true? true-false hyp))
          (and (accepted? ws hyp)
               (not (tf-true? true-false hyp))
               ;; check that some true rival was rejected due to
               ;; minscore
               (some (fn [h] (and (tf-true? true-false h)
                              (rejected? ws h)
                              (= :minscore (rejection-reason ws h))))
                  (explainers ws (accepted-explained ws hyp)))))
      :minscore
      ;; scoring error: if you were accepted but are false, and one
      ;; of your rivals is true; or you were not accepted but are
      ;; true, and you were the rival when a false explainer was
      ;; accepted
      (or (and (accepted? ws hyp)
               (not (tf-true? true-false hyp))
               (some #(tf-true? true-false %)
                  (accepted-rivals ws hyp)))
          (and (not (accepted? ws hyp))
               (tf-true? true-false hyp)
               (let [accepted-instead-ids (map first
                                             (filter (fn [[hypid rivals]] (#{hyp} rivals))
                                                (seq (:accepted-rivals ws))))]
                 (some #(not (get-in true-false [:individual %])) accepted-instead-ids))))
      :scoring
      ;; false acceptance but true hyp (for what was explained) was never offered
      (and (accepted? ws hyp)
           (not (tf-true? true-false hyp))
           (not-any? #(tf-true? true-false %)
                     (explainers ws (accepted-explained ws hyp))))
      :no-expl-offered
      ;; todo: explain this
      (and (rejected? ws hyp)
           (= :conflict (rejection-reason ws hyp))
           (tf-true? true-false hyp))
      (let [acc-conflicting (filter #(and (not (checked %))
                                     (accepted-before? ws % hyp))
                               (find-conflicts-all ws hyp))
            parent-errors (map #(classify-error ws true-false % (conj checked hyp))
                             acc-conflicting)]
        (cond (some #{:noise} parent-errors) :noise
              (some #{:minscore} parent-errors) :minscore
              (some #{:scoring} parent-errors) :scoring
              (some #{:no-expl-offered} parent-errors) :no-expl-offered
              :else
              :conflict-rejection))
      ;; false but accepted, conflicting but true hyp was rejected,
      ;; but why? check recursively
      (and (accepted? ws hyp)
           (not (tf-true? true-false hyp))
           (some #(and (rejected? ws %) (tf-true? true-false %))
              (find-conflicts-all ws hyp)))
      (let [rej-conflicting (filter #(and (not (checked %))
                                     (rejected-before? ws % hyp)
                                     (tf-true? true-false %))
                               (find-conflicts-all ws hyp))
            parent-errors (map #(classify-error ws true-false % (conj checked hyp))
                             rej-conflicting)]
        (cond (some #{:noise} parent-errors) :noise
              (some #{:minscore} parent-errors) :minscore
              (some #{:scoring} parent-errors) :scoring
              (some #{:no-expl-offered} parent-errors) :no-expl-offered
              :else
              :conflict-rejection))
      ;; a true thing was not accepted because it wasn't needed to explain
      (and (not (accepted? ws hyp))
           (tf-true? true-false hyp)
           (= 0 (:Threshold params))
           (not-any? (fn [e] (unexplained? ws e)) (explains ws hyp)))
      :superfluous
      ;; a false thing was accepted, or true thing not accepted (and
      ;; threshold = 0); must be an order-dependency error if none
      ;; of the above errors are the cause
      (or (and (accepted? ws hyp)
               (not (tf-true? true-false hyp)))
          (and (not (accepted? ws hyp))
               (tf-true? true-false hyp)
               (= 0 (:Threshold params))))
      :unknown
      ;; else, there was no error
      :else
      :no-error)))

(defn find-errors
  [est true-false]
  ;; only need to look at last workspace; it contains all the history
  (let [ws (:workspace (cur-ep est))]
    (frequencies
     (for [hyp (filter #(not= :kb (:type %)) (vals (:hyp-ids ws)))]
       (classify-error ws true-false hyp)))))

(defn classify-noexp-reason
  [ws hyp]
  (let [expl (explainers ws hyp)
        rej-reasons (map #(rejection-reason ws %) expl)]
    (cond
     (empty? expl)
     :no-expl-offered
     (some #{:minscore} rej-reasons)
     :minscore
     :else
     :conflict)))

(defn find-noexp-reasons
  [est]
  (let [ws (:workspace (cur-ep est))
        noexp (map #(lookup-hyp ws %) (get-no-explainers ws))]
    (frequencies
     (for [hyp noexp]
       (classify-noexp-reason ws hyp)))))

(defn true-meta-hyp?
  "Note that hyp may be a non-meta hyp if it's a problem case."
  [truedata hyp]
  (let [t? (partial (:oracle-fn @problem) truedata)]
    (cond (= :meta-rej-minscore (:type hyp))
          (and (not-empty (:resolves hyp))
               (some (fn [h] (t? h)) (:implicated hyp))
               (every? (fn [h] (t? h)) (:resolves hyp)))
          (= :meta-rej-conflict (:type hyp))
          (and (not-empty (:resolves hyp))
               (not (t? (:implicated hyp)))
               (every? t? (:resolves hyp)))
          (= :meta-order-dep (:type hyp))
          (and (not-empty (:resolves hyp))
               (every? t? (:resolves hyp)))
          :else
          (t? hyp))))

(defn find-meta-hyps
  [est]
  (let [eps (flatten-est est)
        meta-eps (mapcat (comp flatten-est :meta-est) (filter :meta-est eps))]
    (set (mapcat (fn [ep] (map #(lookup-hyp (:workspace ep) %)
                            (:all (:accepted (:workspace ep)))))
                 meta-eps))))

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        eps (flatten-est est)
        workspace (update-graph (:workspace ep))
        true-false (group-hyps-by-true-false (vals (:hyp-ids workspace))
                                             :type truedata (:oracle-fn @problem) false)
        true-false-scores (calc-true-false-scores est true-false)
        delta-avgs (calc-true-false-deltas est true-false false)
        meta-hyps (find-meta-hyps est)
        meta-true-false (group-hyps-by-true-false meta-hyps :type
                                                  truedata true-meta-hyp? true)
        meta-true-false-scores (calc-true-false-scores est meta-true-false)
        meta-delta-avgs (calc-true-false-deltas est meta-true-false true)
        ep-states (flatten-est est)
        doubt (doubt-aggregate est)
        errors (find-errors est true-false)
        noexp-reasons (find-noexp-reasons est)
        decision-metrics
        (for [ep (filter :decision-point ep-states)]
          (let [ws (:workspace ep)
                noise-obs (set (filter #(not (tf-true? true-false %))
                                  (filter #(= :observation (:type %))
                                     (vals (:hyp-ids ws)))))
                noise-claims (set (filter #(= :ignoring (rejection-reason ws %))
                                     (filter #(= :observation (:type %))
                                        (vals (:hyp-ids ws)))))
                noise-claims-true (set (filter #(not (tf-true? true-false %))
                                          noise-claims))
                noise-claims-false (set/difference noise-claims noise-claims-true)
                noise-prec-coverage (calc-prec-coverage
                                     (count noise-claims-true)  ;; tp
                                     0                          ;; tn
                                     (count noise-claims-false) ;; fp
                                     0                          ;; fn
                                     ;; and event-count:
                                     (count noise-obs))]
            {:Unexplained (count (get-unexplained ws))
             :UnexplainedPct (get-unexp-pct ws)
             :NoExplainersPct (get-noexp-pct ws)
             :NoiseTotal (count noise-obs)
             :NoiseClaimsTrue (count noise-claims-true)
             :NoiseClaimsFalse (count noise-claims-false)
             :NoiseClaimsPrec (:Prec noise-prec-coverage)
             :NoiseClaimsCoverage (:Coverage noise-prec-coverage)
             :NoiseClaimsF1 (:F1 noise-prec-coverage)}))]
    (merge {:Problem (:name @problem)}
           params
           ((:evaluate-fn (:abduction @problem)) truedata est)
           true-false-scores
           meta-true-false-scores
           (last decision-metrics)
           {:Step (:time ep)
            :AvgUnexplainedPct (avg (map :UnexplainedPct decision-metrics))
            :AvgNoExplainersPct (avg (map :NoExplainersPct decision-metrics))
            :AvgNoiseClaimsPrec (avg (map :NoiseClaimsPrec decision-metrics))
            :AvgNoiseClaimsCoverage (avg (map :NoiseClaimsCoverage decision-metrics))
            :AvgNoiseClaimsF1 (avg (map :NoiseClaimsF1 decision-metrics))
            :TrueDeltaAvg (:true-delta-avg delta-avgs)
            :FalseDeltaAvg (:false-delta-avg delta-avgs)
            :MetaTrueDeltaAvg (:true-delta-avg meta-delta-avgs)
            :MetaFalseDeltaAvg (:false-delta-avg meta-delta-avgs)
            :Doubt (doubt-aggregate est)
            :ExplainCycles (count ep-states)
            :MetaBranches (count-branches est)
            :HypothesisCount ((comp count :hyp-ids :workspace) ep)
            :ErrorsCount (reduce + (vals (dissoc errors :no-error)))
            :ErrorsNoise (:noise errors 0)
            :ErrorsConflictRejection (:conflict-rejection errors 0)
            :ErrorsMinScore (:minscore errors 0)
            :ErrorsScoring (:scoring errors 0)
            :ErrorsNoExpl (:no-expl-offered errors 0)
            :ErrorsSuperfluous (:superfluous errors 0)
            :ErrorsUnknown (:unknown errors 0)
            :ErrorsNoError (:no-error errors 0)
            :NoExpCount (reduce + (vals noexp-reasons))
            :NoExpReasonConflict (:conflict noexp-reasons 0)
            :NoExpReasonMinScore (:minscore noexp-reasons 0)
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
                               :Doubt :ExplainCycles :HypothesisCount
                               :MetaBranches :ErrorsCount :ErrorsNoise
                               :ErrorsConflictRejection :ErrorsMinScore :ErrorsSuperfluous
                               :ErrorsScoring :ErrorsUnknown :ErrorsNoError :NoExpCount
                               :NoExpReasonNoise :NoExpReasonRejectedConflict
                               :NoExpReasonRejectedMinScore :NoExpReasonNoExpl
                               :NoExpReasonUnknown :NoiseTotal
                               :NoiseClaimsTrue :NoiseClaimsFalse
                               :NoiseClaimsPrec :NoiseClaimsCoverage :NoiseClaimsF1]
                              (mapcat
                               (fn [tf]
                                 (map #(keyword
                                      (format "%s%s" tf (keyword-to-metric %)))
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
