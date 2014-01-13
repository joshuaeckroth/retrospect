(ns retrospect.reason.abduction.evaluate
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [loom.graph :only [transpose]])
  (:use [retrospect.epistemicstates
         :only [cur-ep flatten-est count-branches decision-points]])
  (:use [retrospect.evaluate])
  (:use [retrospect.epistemicstates :only [ep-path]])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.state]))

(defn keyword-to-metric
  [kw]
  (apply str (map str/capitalize (str/split (name kw) #"-"))))

(defn doubt-aggregate
  [est]
  (let [doubts (filter identity (map #(calc-doubt %) (map :workspace (ep-path est))))
        noexp (no-explainers (:workspace (cur-ep est)))
        ds (if (= "accgraph" (:DoubtMeasure params))
             doubts
             (concat (mapcat (fn [h] (repeat (:DoubtNoExp params) (:apriori h))) noexp) doubts))]
    (if (empty? ds) 0.0
      (if (= "accgraph" (:DoubtMeasure params))
        (last ds) (avg ds)))))

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
              {} (if meta? (:meta-hyp-types @reasoner)
                     (:hyp-types (:abduction @problem))))
        all-true (mapcat #(get % true) (vals tf))
        all-false (mapcat #(get % false) (vals tf))]
    (assoc tf (if meta? :meta-all :all) {true all-true false all-false})))

(defn calc-true-false-scores
  "Find average scores for true hyps, average scores for false hyps."
  [est true-false]
  (let [workspace (:workspace (cur-ep est))
        eps (flatten-est est)
        meta-eps (mapcat (comp flatten-est :meta-est) (filter :meta-est eps))
        acc? (fn [h] (if ((:meta-hyp-types @reasoner) (:type h))
                       (some (fn [ep] (accepted? (:workspace ep) h)) meta-eps)
                       (accepted? workspace h)))
        aprioris (reduce (fn [m t]
                           (assoc m t
                                  {true (map :apriori (get (get true-false t) true))
                                   false (map :apriori (get (get true-false t) false))}))
                         {} (keys true-false))
        pct-true-in-range (fn [start end t]
                            (let [hyps (filter #(and (<= start (:apriori %))
                                                     (> end (:apriori %)))
                                               (concat (get-in true-false [t true])
                                                       (get-in true-false [t false])))]
                              (if (empty? hyps) Double/NaN
                                  (double (/ (count (filter #(tf-true? true-false %) hyps))
                                             (count hyps))))))]
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
                  (keyword (format "CountAcc%s" k))
                  (count (filter acc? (concat (get (get true-false t) true)
                                              (get (get true-false t) false))))
                  (keyword (format "AvgTrueApriori%s" k))
                  (avg (get (get aprioris t) true))
                  (keyword (format "AvgFalseApriori%s" k))
                  (avg (get (get aprioris t) false))
                  (keyword (format "PctTrue0025%s" k))
                  (pct-true-in-range 0.0 0.25 t)
                  (keyword (format "PctTrue2550%s" k))
                  (pct-true-in-range 0.25 0.50 t)
                  (keyword (format "PctTrue5075%s" k))
                  (pct-true-in-range 0.50 0.75 t)
                  (keyword (format "PctTrue75100%s" k))
                  (pct-true-in-range 0.75 1.01 t))))
            {} (keys (dissoc true-false :individual)))))

(defn calc-true-false-deltas
  "Find average delta for true and false acceptances of each hyp type."
  [est true-false meta?]
  (let [eps (if meta?
              (mapcat (comp flatten-est :meta-est) (filter :meta-est (flatten-est est)))
              (ep-path est))
        delta-tf (for [ep (filter (fn [ep] (when-let [b (:best (:accrej (:workspace ep)))]
                                       (not ((:ignore-doubt-types (:abduction @problem))
                                             (:type b)))))
                             eps)]
                   (let [accrej (:accrej (:workspace ep))]
                     {:type (:type (:best accrej))
                      :tf (tf-true? true-false (:best accrej))
                      :delta (:delta accrej)}))
        grouped-delta-tf (group-by :type delta-tf)]
    (reduce (fn [m t]
         (let [ds (get grouped-delta-tf t [])
               k (keyword-to-metric t)]
           (assoc m
             (keyword (format "TrueDeltaAvg%s" k)) (avg (map :delta (filter :tf ds)))
             (keyword (format "FalseDeltaAvg%s" k)) (avg (map :delta (filter #(not (:tf %)) ds))))))
       {(keyword (format "True%sDeltaAvg" (if meta? "Meta" ""))) (avg (map :delta (filter :tf delta-tf)))
        (keyword (format "False%sDeltaAvg" (if meta? "Meta" ""))) (avg (map :delta (filter #(not (:tf %)) delta-tf)))}
       (keys (dissoc true-false :all :individual)))))

(defn calc-true-false-explained
  "Find average number of explainers, average score of best, and average delta
   for true and false evidence."
  [est true-false meta?]
  (let [eps (if meta?
              (mapcat (comp flatten-est :meta-est) (filter :meta-est (flatten-est est)))
              (ep-path est))
        acc-tf (group-by :tf? (for [ep (filter (fn [ep] (:best (:accrej (:workspace ep)))) eps)]
                                (let [accrej (:accrej (:workspace ep))]
                                  {:tf? (tf-true? true-false (:explained accrej))
                                   :expcount (inc (count (:alts accrej)))
                                   :score (:apriori (:best accrej))
                                   :delta (:delta accrej)})))]
    (into {} (for [tf [true false] cat [:expcount :score :delta]]
               [(keyword (format "Explained%s%s%sAvg"
                            (if meta? "Meta" "")
                            (str/capitalize (str tf))
                            (str/capitalize (str (name cat)))))
                (avg (map cat (get acc-tf tf)))]))))

(defn classify-error
  ([ws true-false hyp]
     (classify-error ws true-false hyp #{}))
  ([ws true-false hyp checked]
     (let [acc-expl (accepted-explained ws hyp)]
       (cond
        ;; an ignored but true hyp
        (and (tf-true? true-false hyp)
             (= :ignoring (rejection-reason ws hyp)))
        :ignored
        ;; obs that's false yet was accepted, should have been
        ;; ignored/rejected; or a false accepted that explained noise
        (or (and (= :observation (:type hyp))
                 (accepted? ws hyp)
                 (not (tf-true? true-false hyp)))
            (and (not= :observation (:type hyp))
                 (accepted? ws hyp)
                 (not (tf-true? true-false hyp))
                 (not (checked acc-expl))
                 (= :noise (classify-error ws true-false acc-expl (conj checked acc-expl)))))
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
                       (explainers ws acc-expl)))
            (and (accepted? ws hyp)
                 (not (tf-true? true-false hyp))
                 (not (checked acc-expl))
                 (= :minscore (classify-error ws true-false acc-expl (conj checked acc-expl)))))
        :minscore
        ;; scoring error: if you were accepted but are false, and one
        ;; of your rivals is true; or you were not accepted but are
        ;; true, and you were the rival when a false explainer was
        ;; accepted
        (or (and (accepted? ws hyp)              ;; this was accepted,
                 (not (tf-true? true-false hyp)) ;; but it's false,
                 (some #(tf-true? true-false %) (accepted-rivals ws hyp))) ;; and a rival is true; or,
            (and (not (accepted? ws hyp))  ;; this was not accepted,
                 (tf-true? true-false hyp) ;; but it's true, and
                 (some (fn [h] (and (accepted? ws h) ;; another was accepted,
                                    (not (tf-true? true-false h)) ;; which was false,
                                    (some (fn [h2] (= hyp h2)) (accepted-rivals ws h)))) ;; and competed with this one
                       (:all (hypotheses ws)))))
        :scoring
        ;; false acceptance but true hyp (for what was explained) was never offered
        (and (accepted? ws hyp)
             (not (tf-true? true-false hyp))
             (not-any? #(tf-true? true-false %)
                       (explainers ws acc-expl)))
        :no-expl-offered
        ;; recursive check for true hyps that were rejected due to
        ;; conflict; there is some reason the other hyp (that was
        ;; accepted and rejected this one) was accepted, and that
        ;; acceptance was wrong (errorful); so figure out what that
        ;; error was, and then we have an answer to the present error;
        ;; the fallback is conflict-rejection error but that should
        ;; never happen
        (and (rejected? ws hyp)
             (= :conflict (rejection-reason ws hyp))
             (tf-true? true-false hyp))
        (let [acc-conflicting (filter #(and (not (checked %)) (accepted? ws %))
                                      (find-conflicts ws hyp))
              parent-errors (map #(classify-error ws true-false % (conj checked hyp))
                                 acc-conflicting)]
          (cond (some #{:noise} parent-errors) :noise
                (some #{:minscore} parent-errors) :minscore
                (some #{:scoring} parent-errors) :scoring
                (some #{:no-expl-offered} parent-errors) :no-expl-offered
                (some #{:threshold} parent-errors) :threshold
                :else
                :conflict-rejection))
        ;; false but accepted, conflicting but true hyp was rejected,
        ;; but why? check recursively; similar to previous case
        (and (accepted? ws hyp)
             (not (tf-true? true-false hyp))
             (some #(and (rejected? ws %) (tf-true? true-false %))
                   (find-conflicts ws hyp)))
        (let [rej-conflicting (filter #(and (not (checked %)) (rejected? ws %)
                                            (tf-true? true-false %))
                                      (find-conflicts ws hyp))
              parent-errors (map #(classify-error ws true-false % (conj checked hyp))
                                 rej-conflicting)]
          (cond (some #{:noise} parent-errors) :noise
                (some #{:minscore} parent-errors) :minscore
                (some #{:scoring} parent-errors) :scoring
                (some #{:no-expl-offered} parent-errors) :no-expl-offered
                (some #{:threshold} parent-errors) :threshold
                :else
                :conflict-rejection))
        ;; a true thing was not accepted because it wasn't needed to explain
        (and (not (accepted? ws hyp))
             (tf-true? true-false hyp)
             (= 0 (:Threshold params))
             (not-any? (fn [e] (unexplained? ws e)) (explains ws hyp)))
        :superfluous
        ;; a true thing was not accepted due to threshold > 0
        (and (not (accepted? ws hyp))
             (tf-true? true-false hyp)
             (< 0 (:Threshold params)))
        :threshold
        ;; a false thing was accepted, or true thing not accepted (and
        ;; threshold = 0, otherwise previous case would have handled
        ;; it); must be an order-dependency error if none of the above
        ;; errors are the cause
        (or (and (accepted? ws hyp)
                 (not (tf-true? true-false hyp)))
            (and (not (accepted? ws hyp))
                 (tf-true? true-false hyp)))
        (do (println "unknown error:" hyp)
            :unknown)
        ;; else, there was no error
        :else
        :no-error))))

(defn find-errors
  [est true-false]
  ;; only need to look at last workspace; it contains all the history
  (let [ws (:workspace (cur-ep est))]
    (frequencies
     (for [hyp (filter #(not= :kb (:type %)) (vals (:hyp-ids ws)))]
       (classify-error ws true-false hyp)))))

(defn classify-noise
  [ws true-false hyp]
  ;; solitary noise: false observation, and no other observation conlicts with it
  ;; conflicting noise: false observation, and some other observation conflicts with it
  (cond (and (= :observation (:type hyp))
             (not (tf-true? true-false hyp))
             (not-any? #(conflicts? % hyp) (:observation (hypotheses ws))))
        :solitary
        (and (= :observation (:type hyp))
             (not (tf-true? true-false hyp))
             (some #(conflicts? % hyp) (:observation (hypotheses ws))))
        :conflicting
        :else
        :not-noise))

(defn find-noise
  [est true-false]
  (let [ws (:workspace (cur-ep est))]
    (frequencies
     (for [hyp (filter #(not (tf-true? true-false %)) (:observation (hypotheses ws)))]
       (classify-noise ws true-false hyp)))))

(defn noise-noexp-metrics
  [est true-false]
  (let [ws (:workspace (cur-ep est))
        obs (:observation (hypotheses ws))
        noise (filter #(not (tf-true? true-false %)) obs)
        noise-noexp (filter #(no-explainers? ws %) noise)
        noexp (no-explainers ws)
        noexp-noise (filter #(and (= :observation (:type %)) (not (tf-true? true-false %))) noexp)]
    {:NoiseIsNoExpPct (if (empty? noise) Double/NaN (double (/ (count noise-noexp) (count noise))))
     :NoExpIsNoisePct (if (empty? noexp) Double/NaN (double (/ (count noexp-noise) (count noexp))))}))

(defn some-noexp-reason?
  [ws hyp reason]
  (let [expl (explainers ws hyp)
        rej-reasons (map #(rejection-reason ws %) expl)]
    (or (and (= :no-expl-offered reason) (empty? expl))
        (and (= :minscore reason) (some #{:minscore} rej-reasons))
        (and (= :ignored reason) (some #{:ignoring} rej-reasons))
        (and (= :conflict reason) (some #{:conflict} rej-reasons)))))

(defn classify-noexp-reasons
  [ws hyp]
  (let [expl (explainers ws hyp)
        rej-reasons (map #(rejection-reason ws %) expl)]
    (for [reason [:no-expl-offered :minscore :ignored :conflict]
          :when (some-noexp-reason? ws hyp reason)]
      reason)))

(defn count-noexp-reasons
  [est]
  (let [ws (:workspace (cur-ep est))
        noexp (no-explainers ws)]
    (frequencies
     (mapcat #(classify-noexp-reasons ws %) noexp))))

(defn true-meta-hyp?
  "Note that hyp may be a non-meta hyp if it's a problem case."
  [truedata hyp]
  (let [t? (partial (:oracle-fn @problem) truedata)
        resolves-tf (group-by t? (:resolves hyp))
        count-resolves-true (count (get resolves-tf true))
        count-resolves-false (count (get resolves-tf false))]
    (if (cond (= :meta-impl-exp (:type hyp))
              (and (not-empty (:resolves hyp))
                   (>= count-resolves-true count-resolves-false)
                   (t? (:acc-hyp hyp)))
              (= :meta-order-dep (:type hyp))
              (and (not-empty (:resolves hyp))
                   (>= count-resolves-true count-resolves-false))
              (= :meta-insuf-ev (:type hyp))
              (and (not-empty (:resolves hyp))
                   (t? (first (:resolves hyp)))
                   (empty? (filter #(not (t? %)) (:explained-obs hyp))))
              (= :meta-conf-exp (:type hyp))
              (and (not-empty (:resolves hyp))
                   (>= count-resolves-true count-resolves-false)
                   (not (t? (:rej-hyp hyp)))
                   (some t? (:rejected-expl hyp)))
              :else
              (t? hyp))
      true false)))

(defn find-meta-hyps
  [est]
  (let [eps (flatten-est est)
        meta-eps (mapcat (comp flatten-est :meta-est) (filter :meta-est eps))]
    (set (mapcat (fn [ep] (filter (fn [h] ((:meta-hyp-types @reasoner) (:type h)))
                            (map #(lookup-hyp (:workspace ep) %)
                               (:all (:hypotheses (:workspace ep))))))
                 meta-eps))))

(defn meta-hyp-metrics
  [meta-true-false]
  (letfn [(ar-avg-count [type tf]
            (let [vals (map (fn [h] (count (:resolves h)))
                            (get-in meta-true-false [type tf]))]
              (avg vals)))
          (ar-avg-apriori [type tf]
            (let [vals (map (fn [h] (avg (map :apriori (:resolves h))))
                            (get-in meta-true-false [type tf]))]
              (avg vals)))
          (avg-explain-count [type tf]
            (let [vals (map #(count (:explains %)) (get-in meta-true-false [type tf]))]
              (avg vals)))
          (avg-doubt-prior [type tf]
            (let [vals (map :doubt-prior (get-in meta-true-false [type tf]))]
              (avg vals)))
          (avg-doubt-new [type tf]
            (let [vals (map :doubt-new (get-in meta-true-false [type tf]))]
              (avg vals)))
          (avg-doubt-diff [type tf]
            (let [vals (map :doubt-diff (get-in meta-true-false [type tf]))]
              (avg vals)))]
    (reduce (fn [m t]
              (let [k (keyword-to-metric t)]
                (assoc m
                  (keyword (format "TrueAnomalyReduction%s" k)) (ar-avg-count t true)
                  (keyword (format "TrueAnomalyResolvedApriori%s" k)) (ar-avg-apriori t true)
                  (keyword (format "TrueDoubtPrior%s" k)) (avg-doubt-prior t true)
                  (keyword (format "TrueDoubtNew%s" k)) (avg-doubt-new t true)
                  (keyword (format "TrueDoubtDiff%s" k)) (avg-doubt-diff t true)
                  (keyword (format "TrueExplainCount%s" k)) (avg-explain-count t true)
                  (keyword (format "FalseAnomalyReduction%s" k)) (ar-avg-count t false)
                  (keyword (format "FalseAnomalyResolvedApriori%s" k)) (ar-avg-apriori t false)
                  (keyword (format "FalseDoubtPrior%s" k)) (avg-doubt-prior t false)
                  (keyword (format "FalseDoubtNew%s" k)) (avg-doubt-new t false)
                  (keyword (format "FalseDoubtDiff%s" k)) (avg-doubt-diff t false)
                  (keyword (format "FalseExplainCount%s" k)) (avg-explain-count t false))))
            {:TrueMetaImplExpScoreDeltaAvg
             (avg (map :score-delta (get-in meta-true-false [:meta-impl-exp true])))
             :FalseMetaImplExpScoreDeltaAvg
             (avg (map :score-delta (get-in meta-true-false [:meta-impl-exp false])))
             :TrueMetaImplExpConflictsAccepted
             (let [hyps (get-in meta-true-false [:meta-impl-exp true])]
               (if (empty? hyps) 0.0
                   (double (/ (count (filter :conflicts-with-accepted? hyps)) (count hyps)))))
             :FalseMetaImplExpConflictsAccepted
             (let [hyps (get-in meta-true-false [:meta-impl-exp false])]
               (if (empty? hyps) 0.0
                   (double (/ (count (filter :conflicts-with-accepted? hyps)) (count hyps)))))
             :TrueMetaConfExpEpDeltaAvg
             (avg (map :delta (get-in meta-true-false [:meta-conf-exp true])))
             :FalseMetaConfExpEpDeltaAvg
             (avg (map :delta (get-in meta-true-false [:meta-conf-exp false])))
             :TrueMetaConfExpCycleDiffAvg
             (avg (map :cycle-diff (get-in meta-true-false [:meta-conf-exp true])))
             :FalseMetaConfExpCycleDiffAvg
             (avg (map :cycle-diff (get-in meta-true-false [:meta-conf-exp false])))
             :TrueMetaConfExpTimeDiffAvg
             (avg (map :time-diff (get-in meta-true-false [:meta-conf-exp true])))
             :FalseMetaConfExpTimeDiffAvg
             (avg (map :time-diff (get-in meta-true-false [:meta-conf-exp false])))
             :TrueMetaConfExpEssentialCount
             (count (filter :essential? (get-in meta-true-false [:meta-conf-exp true])))
             :FalseMetaConfExpEssentialCount
             (count (filter :essential? (get-in meta-true-false [:meta-conf-exp false])))}
            (:meta-hyp-types @reasoner))))

(defn meta-hyp-workspace-metrics
  [meta-true-false est]
  (let [eps (flatten-est est)
        meta-eps (mapcat (comp flatten-est :meta-est) (filter :meta-est eps))
        meta-hyp-acceptances (filter #((:meta-hyp-types @reasoner) (:type (:best %)))
                                (map (comp :accrej :workspace) meta-eps))
        essential-counts (reduce (fn [m t] (let [acc-t (filter #(= t (:type (:best %))) meta-hyp-acceptances)]
                                       (reduce (fn [m acc]
                                            (if (nil? (:nbest acc))
                                              (update-in m [t :essential] conj (:best acc))
                                              (update-in m [t :non-essential] conj (:best acc))))
                                          m acc-t)))
                            {} (:meta-hyp-types @reasoner))]
    (apply merge
           ;; calculate number of meta-hyps accepted in one meta-abd cycle
           {:MetaHypsAcceptedCountAvg
            (if (empty? (filter :meta-est eps)) 0.0
                (double (/ (reduce + (map (fn [ep] (count (filter #((:meta-hyp-types @reasoner) (:type %))
                                                     (:all (accepted (:workspace ep))))))
                                   ;; filter out cycles that had no "observations"
                                   (filter #(not-empty (accepted (:workspace %)))
                                      (map (comp last flatten-est :meta-est) (filter :meta-est eps)))))
                           (count (filter :meta-est eps)))))}
           (for [t (:meta-hyp-types @reasoner)]
             (let [t-acc (filter #(= t (:type (:best %))) meta-hyp-acceptances)
                   ;; if t-acc is empty, the numerators will all be zero
                   t-acc-count (if (empty? t-acc) 1 (count t-acc))
                   true-essential (filter #(tf-true? meta-true-false %)
                                     (get-in essential-counts [t :essential]))
                   false-essential (filter #(not (tf-true? meta-true-false %))
                                      (get-in essential-counts [t :essential]))
                   true-non-essential (filter #(tf-true? meta-true-false %)
                                         (get-in essential-counts [t :non-essential]))
                   false-non-essential (filter #(not (tf-true? meta-true-false %))
                                          (get-in essential-counts [t :non-essential]))]
               {(keyword (format "TrueEssential%s" (keyword-to-metric t)))
                (double (/ (count true-essential) t-acc-count))
                (keyword (format "FalseEssential%s" (keyword-to-metric t)))
                (double (/ (count false-essential) t-acc-count))
                (keyword (format "TrueNonEssential%s" (keyword-to-metric t)))
                (double (/ (count true-non-essential) t-acc-count))
                (keyword (format "FalseNonEssential%s" (keyword-to-metric t)))
                (double (/ (count false-non-essential) t-acc-count))})))))

(defn noexp-conflict-true-false
  "How many conflict noexp anomalies are true (and should be explained)?"
  [est true-false]
  (let [ws (:workspace (cur-ep est))
        noexp-conflict (filter (fn [h] (= :conflict (some-noexp-reason? ws h :conflict)))
                          (no-explainers ws))
        grouped (group-by #(tf-true? true-false %) noexp-conflict)]
    (if (empty? noexp-conflict)
      ;; todo: return nil maybe?
      {:PctTrueNoExpConflict 0.0
       :PctFalseNoExpConflict 0.0}
      {:PctTrueNoExpConflict (double (/ (count (get grouped true))
                                        (count noexp-conflict)))
       :PctFalseNoExpConflict (double (/ (count (get grouped false))
                                         (count noexp-conflict)))})))

(defn hyp-relatedness
  [workspace]
  (let [hyps (:all (hypotheses workspace))
        acc-rej (concat (:all (accepted workspace))
                        (filter #(= :conflict (rejection-reason workspace %)) (:all (rejected workspace))))
        g (transpose (:hypgraph workspace))
        hyp-count (count hyps)
        rel-counts (map (fn [hyp] (count (related-hyps workspace g hyp acc-rej))) hyps)]
    (avg (map (fn [c] (double (/ c hyp-count))) rel-counts))))

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        eps (flatten-est est)
        workspace (:workspace ep)
        true-false (group-hyps-by-true-false (vals (:hyp-ids workspace))
                                             :type truedata (:oracle-fn @problem) false)
        true-false-scores (calc-true-false-scores est true-false)
        delta-avgs (calc-true-false-deltas est true-false false)
        explained-avgs (calc-true-false-explained est true-false false)
        meta-hyps (find-meta-hyps est)
        meta-true-false (group-hyps-by-true-false meta-hyps :type
                                                  truedata true-meta-hyp? true)
        meta-true-false-scores (calc-true-false-scores est meta-true-false)
        meta-delta-avgs (calc-true-false-deltas est meta-true-false true)
        meta-explained-avgs (calc-true-false-explained est meta-true-false true)
        ep-states (flatten-est est)
        doubt (doubt-aggregate est)
        errors (find-errors est true-false)
        noise-types (find-noise est true-false)
        noise-total (+ (:solitary noise-types 0) (:conflicting noise-types 0))
        noexp-reasons (count-noexp-reasons est)
        decision-metrics
        (for [ep (decision-points est)]
          (let [ws (:workspace ep)
                noise-possible-candidates (filter #(not (tf-true? true-false %)) (:all (hypotheses ws)))
                noise-candidates (if-let [f (:find-noise-hyps-fn (:abduction @problem))]
                                   (set (f noise-possible-candidates))
                                   (set (filter #(= :observation (:type %)) noise-possible-candidates)))
                noise-claims (set (filter #(or (rejected? ws %)
                                               (unexplained? ws %)) noise-candidates))
                not-noise-claims (set/difference noise-candidates noise-claims)
                noise-claims-true (set (filter #(not (tf-true? true-false %)) noise-claims))
                noise-claims-true-solitary (filter #(= :solitary (classify-noise ws true-false %)) noise-claims-true)
                noise-claims-true-conflicting (filter #(= :conflicting (classify-noise ws true-false %)) noise-claims-true)
                noise-claims-false (set/difference noise-claims noise-claims-true)
                not-noise-claims-true (set (filter #(tf-true? true-false %) not-noise-claims))
                not-noise-claims-false (set (filter #(not (tf-true? true-false %)) not-noise-claims))
                not-noise-claims-false-solitary (filter #(= :solitary (classify-noise ws true-false %)) not-noise-claims-false)
                not-noise-claims-false-conflicting (filter #(= :conflicting (classify-noise ws true-false %)) not-noise-claims-false)
                noise-prec-recall (calc-prec-recall
                                     (count noise-claims-true) ;; tp
                                     (count not-noise-claims-true) ;; tn
                                     (count noise-claims-false) ;; fp
                                     (count not-noise-claims-false) ;; fn
                                     ;; and event-count:
                                     (count noise-candidates))]
            {:Unexplained (count (unexplained ws))
             :UnexplainedPct (get-unexp-pct ws)
             :Coverage (- 1.0 (get-unexp-pct-no-obs ws))
             :NoExplainers (count (no-explainers ws))
             :NoExplainersPct (get-noexp-pct ws)
             :NoiseTotal (count noise-candidates)
             :NoiseClaimsTrue (count noise-claims-true)
             :NoiseClaimsFalse (count noise-claims-false)
             :NoiseClaimsPrec (:Prec noise-prec-recall)
             :NoiseClaimsRecall (:Recall noise-prec-recall)
             :NoiseClaimsF1 (:F1 noise-prec-recall)
             :NoiseClaimsTPR (:TPR noise-prec-recall)
             :NoiseClaimsFPR (:FPR noise-prec-recall)
             :NoiseClaimsTrueSolitaryPct (if (empty? noise-claims-true) Double/NaN
                                             (double (/ (count noise-claims-true-solitary)
                                                        (count noise-claims-true))))
             :NoiseClaimsTrueConflictingPct (if (empty? noise-claims-true) Double/NaN
                                                (double (/ (count noise-claims-true-conflicting)
                                                           (count noise-claims-true))))
             :NotNoiseClaimsFalseSolitaryPct (if (empty? not-noise-claims-false) Double/NaN
                                                 (double (/ (count not-noise-claims-false-solitary)
                                                            (count not-noise-claims-false))))
             :NotNoiseClaimsFalseConflictingPct (if (empty? not-noise-claims-false) Double/NaN
                                                    (double (/ (count not-noise-claims-false-conflicting)
                                                               (count not-noise-claims-false))))}))]
    (merge {:Problem (:name @problem)}
           params
           ((:evaluate-fn (:abduction @problem)) truedata est)
           true-false-scores
           meta-true-false-scores
           (meta-hyp-metrics meta-true-false)
           (meta-hyp-workspace-metrics meta-true-false est)
           (noexp-conflict-true-false est true-false)
           (noise-noexp-metrics est true-false)
           explained-avgs
           meta-explained-avgs
           delta-avgs
           meta-delta-avgs
           (last decision-metrics)
           {:Step (:time ep)
            :CallsToObserve (get @calls-to-observe (:simulation params))
            :CallsToHypothesize (get @calls-to-hypothesize (:simulation params))
            :AvgUnexplainedPct (avg (map :UnexplainedPct decision-metrics))
            :AvgCoverage (avg (map :Coverage decision-metrics))
            :AvgNoExplainersPct (avg (map :NoExplainersPct decision-metrics))
            :AvgNoiseClaimsPrec (avg (map :NoiseClaimsPrec decision-metrics))
            :AvgNoiseClaimsRecall (avg (map :NoiseClaimsRecall decision-metrics))
            :AvgNoiseClaimsF1 (avg (map :NoiseClaimsF1 decision-metrics))
            :AvgNoiseClaimsTPR (avg (map :NoiseClaimsTPR decision-metrics))
            :AvgNoiseClaimsFPR (avg (map :NoiseClaimsFPR decision-metrics))
            :Doubt (doubt-aggregate est)
            :ExplainCycles (count ep-states)
            :MetaBranches (count-branches est)
            :HypothesisCount ((comp count :hyp-ids :workspace) ep)
            :ErrorsCount (reduce + (vals (dissoc errors :no-error)))
            :ErrorsNoise (:noise errors 0)
            :ErrorsConflictRejection (:conflict-rejection errors 0)
            :ErrorsMinPlausibility (:minscore errors 0)
            :ErrorsPlausibility (:scoring errors 0)
            :ErrorsNoExpl (:no-expl-offered errors 0)
            :ErrorsSuperfluous (:superfluous errors 0)
            :ErrorsIgnored (:ignored errors 0)
            :ErrorsDeltaThreshold (:threshold errors 0)
            :ErrorsUnknown (:unknown errors 0)
            :ErrorsNoError (:no-error errors 0)
            :NoExpCount (reduce + (vals noexp-reasons))
            :NoExpReasonConflict (:conflict noexp-reasons 0)
            :NoExpReasonIgnored (:ignored noexp-reasons 0)
            :NoExpReasonMinPlausibility (:minscore noexp-reasons 0)
            :NoExpReasonNoExpl (:no-expl-offered noexp-reasons 0)
            :NoiseSolitary (:solitary noise-types 0)
            :NoiseSolitaryPct (if (= 0 noise-total) Double/NaN
                                  (double (/ (:solitary noise-types 0)
                                             noise-total)))
            :NoiseConflicting (:conflicting noise-types 0)
            :NoiseConflictingPct (if (= 0 noise-total) Double/NaN
                                     (double (/ (:conflicting noise-types 0)
                                                noise-total)))})))

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
                               :TrueDeltaAvg :FalseDeltaAvg :Coverage
                               :Doubt :ExplainCycles :HypothesisCount
                               :MetaBranches :ErrorsCount :ErrorsNoise :ErrorsDeltaThreshold
                               :ErrorsConflictRejection :ErrorsMinPlausibility :ErrorsSuperfluous
                               :ErrorsPlausibility :ErrorsUnknown :ErrorsNoExpl :ErrorsNoError
                               :NoExpCount :NoExpReasonNoise :NoExpReasonConflict
                               :NoExpReasonMinPlausibility :NoExpReasonNoExpl
                               :NoiseTotal :NoiseClaimsTPR :NoiseClaimsFPR
                               :NoiseClaimsTrue :NoiseClaimsFalse
                               :NoiseClaimsPrec :NoiseClaimsRecall :NoiseClaimsF1]
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
