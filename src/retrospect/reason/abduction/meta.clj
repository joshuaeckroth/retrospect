(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.math.combinatorics :only [combinations]])
  (:use [retrospect.epistemicstates])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.evaluate :only [avg]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate classify-noexp-reason]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(declare metareason)

(defn find-anomalies
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (no-explainers workspace)))

(defn metareasoning-activated?
  [est]
  (and (not= "none" (:Metareasoning params))
       (not-empty (find-anomalies est))))

(defn est-workspace-child
  [est workspace]
  (new-child-ep (update-est est (assoc (cur-ep est)
                                  :workspace workspace))))

(defn workspace-update-hypotheses
  [workspace time-prev time-now sensors cycle]
  (binding [reason-log (ref '())]
    (let [ws-sensors (if sensors
                       (add-sensor-hyps workspace time-prev time-now sensors cycle)
                       workspace)
          ws-hyps (if (or sensors (:GetMoreHyps params))
                    (update-hypotheses ws-sensors cycle time-now)
                    ws-sensors)]
      (assoc ws-hyps :log @reason-log))))

(defn workspace-explain
  [workspace cycle time-now]
  (binding [reason-log (ref (:log workspace))]
    (log "Explaining at cycle" cycle)
    (let [ws (explain workspace cycle)]
      (assoc ws :log @reason-log))))

(defn explain-and-advance
  [est time-prev time-now sensors]
  (let [ws (:workspace (cur-ep est))
        cycle (:cycle (cur-ep est))
        ws-hyps (workspace-update-hypotheses ws time-prev time-now sensors cycle)
        ws-explained (workspace-explain ws-hyps cycle time-now)
        est-result (est-workspace-child est ws-explained)]
    (if (or (and (:GetMoreHyps params)
                 (not= (count (:hyp-ids ws-explained))
                       (count (:hyp-ids ws))))
            (:best (:accrej ws-explained)))
      ;; don't recur with sensors so that sensor hyps are not re-added
      (recur est-result time-prev time-now nil)
      est-result)))

(defn reason
  [est time-prev time-now sensors & opts]
  (loop [est est]
    (let [est-new (explain-and-advance est time-prev time-now sensors)
          meta? (and (not-any? #{:no-metareason} opts)
                     (metareasoning-activated? est-new))
          est-meta (if (not meta?) est-new
                       (metareason est-new time-prev time-now sensors))]
      ;; if something was accepted last, repeat
      (if (:best (:accrej (:workspace (cur-ep est-meta))))
        (recur est-meta) est-meta))))

(defn meta-apply-and-evaluate
  [est est-new time-now sensors]
  (let [reason-est (reason est-new (:time (cur-ep est-new))
                           time-now sensors :no-metareason)]
    {:est-old (goto-ep reason-est (:id (cur-ep est)))
     :est-new reason-est}))

(defn resolve-false-evidence
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ;; don't undecide related hyps; we want to make as little change to
        ;; the ep state as possible so that we don't wind up with a different noexp
        ws-undecided (reduce (fn [ws hyp] (undecide ws hyp (:cycle ep) false))
                             (:workspace ep) implicated)
        ws-ignored (reduce (fn [ws hyp] (reject ws hyp :ignoring (:cycle ep)))
                           ws-undecided implicated)
        ep-ignored (assoc ep :workspace ws-ignored)]
    [(update-est new-est ep-ignored) params]))

(defn resolve-conflicting-explainers
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-undecided (reduce (fn [ws hyp] (undecide ws hyp (:cycle ep)))
                             (:workspace ep) implicated)
        ws-rejected (reduce (fn [ws hyp]
                              (-> ws
                                  (prevent-undecide hyp)
                                  (reject hyp :preemptive (:cycle ep))))
                            ws-undecided implicated)
        ep-rejected (assoc ep :workspace ws-rejected)]
    [(update-est new-est ep-rejected) params]))

(defn resolve-implausible-explainers
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-undecided (reduce (fn [ws hyp] (undecide ws hyp (:cycle ep)))
                             (:workspace ep) implicated)
        ws-prev-minscore (reduce (fn [ws hyp]
                                   (-> ws (prevent-undecide hyp)
                                       (prevent-rejection hyp :minscore)))
                                 ws-undecided implicated)
        ep-prev-minscore (assoc ep :workspace ws-prev-minscore)]
    [(update-est new-est ep-prev-minscore) params]))

(defn resolve-implausible-evidence
  [est]
  (let [old-ep (cur-ep est)
        old-ws (:workspace (cur-ep est))
        ;; restore all observations rejected due to minscore
        implicated (filter (fn [obs] (= :minscore (rejection-reason old-ws obs)))
                           (:observation (rejected old-ws)))
        ws-all-restored (reduce (fn [ws hyp]
                                  (-> ws (undecide hyp (:cycle old-ep))
                                      (accept hyp nil [] [] 0.0 {} (:cycle old-ep))))
                                old-ws implicated)
        ;; generate new explainers
        ws-new-exp (update-hypotheses ws-all-restored (:cycle old-ep) (:time old-ep))
        ;; gather newly-added explainers of anomalies
        rel-anomalies (filter #(= :no-expl-offered (classify-noexp-reason old-ws %)) (find-anomalies est))
        new-exp (set (mapcat (fn [anomaly] (explainers ws-new-exp anomaly)) rel-anomalies))
        ;; collect minscore-rejected observations that new-exp explain (which include rel-anomalies)
        obs-new-exp (filter (fn [obs] (= :minscore (rejection-reason old-ws obs)))
                            (set (mapcat (fn [hyp] (explains ws-new-exp hyp)) new-exp)))
        ;; finally, add back just these observations
        new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-subset-restored (reduce (fn [ws hyp]
                                     (-> ws (undecide hyp (:cycle ep))
                                         (accept hyp nil [] [] 0.0 {} (:cycle ep))))
                                   (:workspace ep) obs-new-exp)
        ep-subset-restored (assoc ep :workspace ws-subset-restored)]
    [(update-est new-est ep-subset-restored) params]))

(defn resolve-implausible-evidence-cleanup
  [est est-prior]
  (let [old-ws (:workspace (cur-ep est-prior))
        unrejected (map :id (filter (fn [h] (= :minscore (rejection-reason old-ws h)))
                                    (:observation (rejected old-ws))))
        new-est (new-child-ep est)
        ep (cur-ep new-est)
        anomalies (map :id (find-anomalies est))
        unrejected-noexp (set/intersection (set unrejected) (set anomalies))
        ;; re-reject the stuff we unrejected (from minscore) but now is a noexp
        ws-rejected (reduce (fn [ws hyp] (reject ws hyp :minscore (:cycle ep)))
                            (:workspace ep) (map #(lookup-hyp (:workspace ep) %) unrejected-noexp))
        ep-cleaned-up (assoc ep :workspace ws-rejected)]
    (update-est new-est ep-cleaned-up)))

(defn find-conflicting-explainers-candidates
  [anomalies est time-now]
  (let [cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) anomalies)) ;; explainers of anomalies
        ;; rejected explainers due to conflict
        expl-rc (set (filter (fn [h] (= :conflict (rejection-reason cur-ws h))) expl))
        acc (set (filter (fn [c] (accepted? cur-ws c)) ;; accepted that conflict with any of expl-rc
                         (set (mapcat #(find-conflicts cur-ws %) expl-rc))))
        inner-hyps (set (map :id (mapcat :hyps acc))) ;; inner hyps, if any, of acc
        ;; keep only those that are not inner hyps
        acc-no-inner (sort-by :id (filter #(not (inner-hyps (:id %))) acc)) 
        acc-no-inner-ids (set (map :id acc-no-inner))
        ;; may have been accepted multiple times, if undecided between; want the earliest time
        ep-rejs (filter (fn [ep] (some acc-no-inner-ids (:acc (:accrej (:workspace ep))))) (ep-path est))
        ep-rejs-deltas (map (fn [ep] {:delta (get-in ep [:workspace :accrej :delta])
                                      :cycle (:cycle ep)
                                      :hyp (get-in ep [:workspace :accrej :best])})
                            ep-rejs)
        earliest-rejs-deltas (for [hyp (sort-by :id (map :hyp ep-rejs-deltas))]
                               (first (sort-by :cycle (filter #(= hyp (:hyp %)) ep-rejs-deltas))))]
    (filter #(not-empty (:may-resolve %))
            (for [{:keys [delta cycle hyp]} earliest-rejs-deltas]
              (let [expl-conf (filter #(conflicts? hyp %) expl-rc)
                    expl-explained (set (mapcat :explains expl-conf))
                    ;; anomalies explained by expl-conf, and therefore possibly resolved
                    pc-res (filter (fn [pc] (expl-explained (:contents pc))) anomalies)]
                {:implicated hyp :cycle cycle :delta delta :may-resolve (sort-by :id pc-res)
                 :rejected (sort-by :id expl-conf)})))))

(defn find-implausible-explainers-candidates
  [anomalies est time-now]
  (let [cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) anomalies)) ;; explainers of anomalies
        expl-rejected-minscore (sort-by :id (filter (fn [h] (= :minscore (rejection-reason cur-ws h))) expl))]
    (filter #(not-empty (:may-resolve %))
            (for [e expl-rejected-minscore]
              {:implicated e
               :may-resolve (sort-by :id (filter (fn [pc] (some #{(:contents pc)} (:explains e)))
                                                 anomalies))}))))

(defn meta-implausible-explainers
  [anomalies est-orig time-prev time-now sensors]
  (loop [anomalies anomalies
         est-prior est-orig
         est est-orig
         implicated-before #{}]
    (let [cur-ws (:workspace (cur-ep est))
          ;; take the greatest scoring hyp to prevent-rejection
          {:keys [implicated]} (last (sort-by (comp :apriori :implicated)
                                              (find-implausible-explainers-candidates anomalies est time-now)))]
      (if (and implicated (not (implicated-before (:contents implicated))))
        (let [[est-action params-action] (resolve-implausible-explainers [implicated] est)
              {:keys [est-old est-new]} (binding [params params-action]
                                          (meta-apply-and-evaluate est est-action time-now sensors))
              anomalies-new (find-anomalies est-new)]
          (if (not-empty anomalies-new)
            (recur anomalies-new est-old est-new (conj implicated-before (:contents implicated)))
            {:est-old est-old :est-new est-new}))
        {:est-old est-prior :est-new est}))))

(defn meta-hyp-conflicts?
  [ws hyp1 hyp2]
  (related-hyps? ws (:implicated hyp1) (:implicated hyp2)))

(defn make-meta-hyps-conflicting-explainers
  [anomalies est time-prev time-now available-meta-hyps]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (if (not (available-meta-hyps "meta-conf-exp")) []
      (let [rel-prob-cases (filter #(= :conflict (classify-noexp-reason (:workspace (cur-ep est)) %))
                              anomalies)]
        (for [{:keys [implicated cycle rejected delta may-resolve]}
              (find-conflicting-explainers-candidates rel-prob-cases est time-now)]
          (new-hyp "ConfExp" :meta-conf-exp :meta-conf-exp
                   0.0 false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
                   (map :contents may-resolve)
                   (format "%s rejected some explainers" implicated)
                   (format "%s rejected these explainers (cycle %d, delta %.2f):\n%s"
                      (str implicated) cycle delta (str/join "\n" (map str rejected)))
                   {:action (partial resolve-conflicting-explainers [implicated])
                    :resolves may-resolve
                    :rejected rejected
                    :cycle cycle
                    :delta delta
                    :implicated implicated})))))

(defn make-meta-hyps-implausible-explainers
  [anomalies est time-prev time-now available-meta-hyps]
  ;; were some explainers omitted due to high min-score?
  (if (not (available-meta-hyps "meta-impl-exp")) []
      (let [rel-prob-cases (filter #(= :minscore (classify-noexp-reason (:workspace (cur-ep est)) %))
                                   anomalies)
            candidates (find-implausible-explainers-candidates rel-prob-cases est time-now)
            implicated (map :implicated candidates)
            may-resolve (set (mapcat :may-resolve candidates))
            conflicts-with-accepted? (every? (fn [h]
                                               (some (partial conflicts? h)
                                                     (:all (accepted (:workspace (cur-ep est))))))
                                             implicated)]
        (if (and (not= "oracle" (:Metareasoning params))
                 (:RemoveConflictingImplExp params)
                 conflicts-with-accepted?)
          []
          [(new-hyp "ImplExp" :meta-impl-exp :meta-impl-exp
                    0.0 false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
                    (map :contents may-resolve)
                    "Explainer rejected due to too-high min-score"
                    (format (str "These explainers were rejected due to too-high min-score:\n"
                                 "%s\n\nRelevant problem cases:\n%s\n\nConflicts with accepted? %s")
                            (str/join "\n" (sort (map str implicated)))
                            (str/join "\n" (sort (map str may-resolve)))
                            (str conflicts-with-accepted?))
                    {:action (partial resolve-implausible-explainers implicated)
                     :resolves may-resolve
                     :implicated implicated})]))))

(defn make-meta-hyps-implausible-evidence
  [anomalies est time-prev time-now available-meta-hyps]
  (if (not (available-meta-hyps "meta-impl-ev")) []
      (let [rel-prob-cases (filter #(= :no-expl-offered (classify-noexp-reason (:workspace (cur-ep est)) %))
                                   anomalies)]
        [(new-hyp "ImplEv" :meta-impl-ev :meta-impl-ev
                  0.0 false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
                  (map :contents rel-prob-cases)
                  "Explainers never offered due to rejection of implausible evidence"
                  "Explainers never offered due to rejection of implausible evidence"
                  {:action resolve-implausible-evidence
                   :cleanup resolve-implausible-evidence-cleanup
                   :resolves rel-prob-cases})])))

(defn make-meta-hyps
  "Create explanations, and associated actions, for anomalies."
  [anomalies est time-prev time-now]
  (let [available-meta-hyps (set (str/split (:MetaHyps params) #","))]
    (apply concat
           (for [meta-fn [make-meta-hyps-conflicting-explainers
                          make-meta-hyps-implausible-explainers
                          make-meta-hyps-implausible-evidence]]
             (meta-fn anomalies est time-prev time-now available-meta-hyps)))))

(defn score-meta-hyps-estimate
  [anomalies meta-hyps est time-prev time-now sensors]
  ;; must use (doall) to avoid lazy evaluation since state/params may
  ;; change later (specifically, MinScore and Threshold change to
  ;; MetaMinScore and MetaThreshold)
  [est (doall meta-hyps)])

(defn score-meta-hyps-simulate-apriori
  [hyp anomalies anomalies-new doubt doubt-new]
  (cond (= "apriori-diff" (:ScoreMetaHyps params))
        (max 0.0 (- (avg (map :apriori anomalies))
                    (avg (map :apriori anomalies-new))))
        (= "doubt-diff" (:ScoreMetaHyps params))
        (max 0.0 (- doubt doubt-new))
        ;; "doubt"
        :else
        doubt-new))

(defn score-meta-hyps-simulate
  [anomalies meta-hyps est time-prev time-now sensors]
  (loop [est-attempted est
         hyps meta-hyps
         new-hyps []]
    (if (empty? hyps) [(goto-ep est-attempted (:id (cur-ep est))) new-hyps]
        (let [hyp (first hyps)
              [est-new params-new] ((:action hyp) est-attempted)
              result-nocleanup (binding [params params-new]
                                 (meta-apply-and-evaluate est-attempted est-new time-now sensors))
              result (if-let [cleanup (:cleanup hyp)]
                       (update-in result-nocleanup [:est-new] cleanup est-attempted)
                       result-nocleanup)
              doubt (doubt-aggregate est)
              doubt-new (doubt-aggregate (:est-new result))
              anomalies-new (find-anomalies (:est-new result))
              resolved-cases (filter (fn [pc] (not ((set (map :contents anomalies-new))
                                                    (:contents pc))))
                                     anomalies)]
          (recur (:est-old result) (rest hyps)
                 (conj new-hyps
                       (assoc hyp :explains (map :contents resolved-cases)
                              :resolves resolved-cases
                              :anomalies-prior anomalies
                              :anomalies-after anomalies-new
                              :final-ep-id (:id (cur-ep (:est-new result)))
                              :apriori (score-meta-hyps-simulate-apriori
                                        hyp anomalies anomalies-new
                                        doubt doubt-new)
                              :doubt-prior doubt
                              :doubt-new doubt-new
                              :doubt-diff (- doubt-new doubt)
                              :desc (format (str "%s\n\nEp-state start: %s\n\n"
                                                 "Problem cases prior:\n%s\n\n"
                                                 "Problem cases after:\n%s\n\n"
                                                 "Doubt before: %.2f\n"
                                                 "Doubt after: %.2f\n"
                                                 "Avg apriori of problem cases prior: %.2f\n"
                                                 "Avg apriori of problem cases after: %.2f\n"
                                                 "Avg apriori diff: %.2f")
                                            (:desc hyp) (str (cur-ep est-new))
                                            (str/join "\n" (sort-by :id anomalies))
                                            (str/join "\n" (sort-by :id anomalies-new))
                                            doubt
                                            doubt-new
                                            (avg (map :apriori anomalies))
                                            (avg (map :apriori anomalies-new))
                                            (- (avg (map :apriori anomalies))
                                               (avg (map :apriori anomalies-new)))))))))))

(defn score-meta-hyps
  [anomalies meta-hyps est time-prev time-now sensors]
  (let [scorer (if (:EstimateMetaScores params)
                 score-meta-hyps-estimate
                 score-meta-hyps-simulate)]
    (scorer anomalies meta-hyps est time-prev time-now sensors)))

(defn meta-abductive
  [anomalies est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps anomalies est time-prev time-now)
        [est-new meta-hyps-scored] (score-meta-hyps anomalies meta-hyps est time-prev time-now sensors)
        meta-ws (if (= "oracle" (:Metareasoning params))
                  (assoc (init-workspace)
                    :meta-oracle (:meta-oracle (:workspace (cur-ep est))))
                  (init-workspace))
        meta-est (new-child-ep (init-est meta-ws))
        meta-params (assoc params
                      :MinScore (if (= "oracle" (:Metareasoning params)) 1
                                    (:MetaMinScore params))
                      :Threshold (:MetaThreshold params)
                      :GetMoreHyps false
                      :AblatePct 0
                      :InvertScoresPct 0)
        meta-ws (binding [params meta-params]
                  (let [ws-obs (reduce (fn [ws h]
                                         (-> ws (add h 0)
                                             (accept h nil [] [] 0.0 {} 0)))
                                       (:workspace (cur-ep meta-est)) anomalies)]
                    (reduce (fn [ws h] (add ws h 0)) ws-obs meta-hyps-scored)))
        meta-est-reasoned (binding [params meta-params]
                            (reason (update-est-ep meta-est :workspace meta-ws) 0 1 nil :no-metareason))]
    (update-est-ep est-new :meta-est meta-est-reasoned)))

(defn meta-abductive-recursive
  [anomalies est time-prev time-now sensors]
  (loop [anomalies anomalies
         est est
         attempted #{}
         implicated #{}]
    (let [est-abd (meta-abductive anomalies est time-prev time-now sensors)
          meta-workspace (:workspace (cur-ep (:meta-est (cur-ep est-abd))))
          meta-accepted (filter (fn [h] (and (not (attempted (dissoc (:contents h) :action)))
                                             (not (implicated (:contents (:implicated h))))))
                                (apply concat (vals (select-keys (accepted meta-workspace)
                                                                 (:meta-hyp-types @reasoner)))))
          est-applied (if (empty? meta-accepted) est-abd
                          (reduce (fn [est hyp]
                                    (let [[est-new params-new] ((:action hyp) est)
                                          est-nocleanup (binding [params params-new]
                                                          (:est-new (meta-apply-and-evaluate
                                                                     est est-new time-now sensors)))]
                                      (if-let [cleanup (:cleanup hyp)]
                                        (cleanup est-nocleanup est)
                                        est-nocleanup)))
                                  est-abd meta-accepted))
          anomalies-new (when (not-empty meta-accepted) (find-anomalies est-applied))]
      (if (and (not-empty meta-accepted) (not-empty anomalies-new)
               (< (count anomalies-new) (count anomalies)))
        (recur anomalies-new
               est-applied
               (set/union attempted (set (map (fn [h] (dissoc (:contents h) :action)) meta-accepted)))
               (set/union implicated (set (map (fn [h] (:contents (:implicated h))) meta-accepted))))
        {:est-old (goto-ep est-applied (:id (cur-ep est))) :est-new est-applied}))))

(defn assume-false-evidence
  [anomalies est time-now sensors]
  (let [[new-est _] (resolve-false-evidence anomalies est)]
    (reason new-est (:time (cur-ep est)) time-now sensors :no-metareason)))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [est time-prev time-now sensors]
  (let [anomalies (find-anomalies est)
        m (:Metareasoning params)
        f (cond (= "only-impl-exp" m)
                meta-implausible-explainers
                (or (= "abd" m) (= "oracle" m))
                meta-abductive-recursive
                (= "assume-false-ev" m)
                (constantly nil))
        result (f anomalies est time-prev time-now sensors)
        anomalies-old (if result (find-anomalies (:est-old result))
                              anomalies)
        anomalies-new (when result (find-anomalies (:est-new result)))]
    (cond (nil? result)
          (assume-false-evidence anomalies-old est time-now sensors)
          (empty? anomalies-new)
          (:est-new result)
          (< (count anomalies-new) (count anomalies-old))
          (assume-false-evidence anomalies-new (:est-new result) time-now sensors)
          :else
          (assume-false-evidence anomalies-old (:est-old result) time-now sensors))))
