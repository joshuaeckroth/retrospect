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

;; basic reasoning process
;;{{{

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

;;}}}

(defn meta-hyp-conflicts?
  [ws hyp1 hyp2]
  (or (= :meta-order-dep (:type hyp1))
      (= :meta-order-dep (:type hyp2))
      (not-empty (set/intersection (set (:explains hyp1)) (set (:explains hyp2))))))

;; conflicting explainers
;;{{{

(defn resolve-conf-exp
  [acc-hyp rej-hyp may-resolve est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws (-> (:workspace ep)
               (undecide rej-hyp (:cycle ep))
               (prevent-undecide rej-hyp)
               (reject rej-hyp :preemptive (:cycle ep))
               (accept acc-hyp nil [] may-resolve nil nil (:cycle ep)))
        ep-acc (assoc ep :workspace ws)]
    [(update-est new-est ep-acc) params]))

(defn conf-exp-candidates
  [anomalies est]
  (let [cur-ws (:workspace (cur-ep est))
        rel-anomalies (filter #(= :conflict (classify-noexp-reason cur-ws %)) anomalies)
        ;; explainers of anomalies
        expl (set (mapcat #(explainers cur-ws %) rel-anomalies))
        ;; rejected explainers due to conflict
        expl-rc (set (filter (fn [h] (= :conflict (rejection-reason cur-ws h))) expl))
        ;; accepted that conflict with any of expl-rc
        acc (set (filter (fn [c] (accepted? cur-ws c))
                         (set (mapcat #(find-conflicts cur-ws %) expl-rc))))
        ;; inner hyps, if any, of acc
        inner-hyps (set (map :id (mapcat :hyps acc))) 
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
            (mapcat (fn [{:keys [delta cycle hyp]}]
                      ;; for each accepted hyp that conflicted with an explainer,
                      ;; get the explainers it conflicted with
                      (for [expl-conf (filter #(conflicts? hyp %) expl-rc)]
                        (let [ ;; anomalies explained by expl-conf, and therefore possibly resolved
                              expl-explained (set (:explains expl-conf))
                              pc-res (filter (fn [pc] (expl-explained (:contents pc))) rel-anomalies)]
                          {:acc-hyp expl-conf :rej-hyp hyp :cycle cycle :delta delta :may-resolve (sort-by :id pc-res)})))
                    earliest-rejs-deltas))))

(defn make-meta-hyps-conflicting-explainers
  [anomalies est _ _]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (for [{:keys [acc-hyp rej-hyp cycle delta may-resolve]} (conf-exp-candidates anomalies est)]
    (new-hyp "ConfExp" :meta-conf-exp :meta-conf-exp
             0.0 false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
             (map :contents may-resolve)
             (format "%s rejected %s" (:name rej-hyp) (:name acc-hyp))
             (format "%s rejected %s at cycle %d with delta %.2f" rej-hyp acc-hyp cycle delta)
             {:action (partial resolve-conf-exp acc-hyp rej-hyp may-resolve)
              :resolves may-resolve
              :acc-hyp acc-hyp
              :rej-hyp rej-hyp
              :implicated rej-hyp
              :cycle cycle
              :delta delta})))
;;}}}

;; implausible explainers
;;{{{

(defn resolve-impl-exp
  [hyp est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws (-> (:workspace ep)
               (undecide hyp (:cycle ep))
               (prevent-undecide hyp)
               (prevent-rejection hyp :minscore))
        ep-prev-minscore (assoc ep :workspace ws)]
    [(update-est new-est ep-prev-minscore) params]))

(defn impl-exp-candidates
  [anomalies est]
  (let [cur-ws (:workspace (cur-ep est))
        rel-anomalies (filter #(= :minscore (classify-noexp-reason cur-ws %)) anomalies)
        ;; explainers of anomalies
        expl (set (mapcat #(explainers cur-ws %) rel-anomalies))
        ;; explainers that were rejected due to minscore
        expl-rejected-minscore (sort-by :id (filter (fn [h] (= :minscore (rejection-reason cur-ws h))) expl))]
    (filter #(not-empty (:may-resolve %))
            (for [hyp expl-rejected-minscore]
              (let [may-resolve (sort-by :id (filter (fn [pc] (some #{(:contents pc)} (:explains hyp)))
                                                     rel-anomalies))]
                {:acc-hyp hyp
                 :may-resolve may-resolve
                 :score-delta (- (/ (:MinScore params) 100.0) (:apriori hyp))})))))

(defn make-meta-hyps-implausible-explainers
  [anomalies est _ _]
  ;; were some explainers omitted due to high min-score?
  (let [candidates (impl-exp-candidates anomalies est)
        meta-hyps (for [{:keys [acc-hyp may-resolve score-delta]} candidates]
                    (let [conflicts-with-accepted? (some (partial conflicts? acc-hyp)
                                                         (:all (accepted (:workspace (cur-ep est)))))]
                      (new-hyp "ImplExp" :meta-impl-exp :meta-impl-exp
                               0.0 false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
                               (map :contents may-resolve)
                               "Explainer rejected due to min-score"
                               (format "%s was rejected due to min-score\n\nConflicts with accepted? %s\nScore delta: %.2f"
                                       acc-hyp (str conflicts-with-accepted?) score-delta)
                               {:action (partial resolve-impl-exp acc-hyp)
                                :resolves may-resolve
                                :acc-hyp acc-hyp
                                :score-delta score-delta
                                :implicated acc-hyp
                                :conflicts-with-accepted? conflicts-with-accepted?})))
        filtered-conflicting (if (and (not= "oracle" (:Metareasoning params))
                                      (:RemoveConflictingImplExp params))
                               (filter #(not (:conflicts-with-accepted? %)) meta-hyps)
                               meta-hyps)
        filtered-score-delta (if (not= "oracle" (:Metareasoning params))
                               (filter #(<= (:score-delta %)
                                            (* (:MaxMetaImplExpScoreDelta params)
                                               (/ (:MinScore params) 100.0)))
                                       filtered-conflicting)
                               filtered-conflicting)]
    filtered-score-delta))

;;}}}

;; sensor report order dependencies
;;{{{

(defn resolve-order-dep
  [ep est]
  [(new-branch-ep est ep) params])

(defn order-dep-candidates
  [anomalies est]
  (let [ws (:workspace (cur-ep est))
        acc (accepted ws)
        ;; gather all observations rejected due to minscore
        possible-evidence (filter (fn [obs] (= :minscore (rejection-reason ws obs)))
                                  (:observation (rejected ws)))
        rel-anomalies (filter (fn [anomaly]
                                (empty? ((:suggest-related-evidence-fn (:abduction @problem))
                                         anomaly possible-evidence acc)))
                              anomalies)
        accept-cycles (into {} (for [hyp rel-anomalies] [hyp (accepted-cycle ws hyp)]))
        time-last (:time (cur-ep est))
        eps (map (fn [t] (cur-ep (goto-start-of-time est t)))
                 (range (max 0 (- time-last (:MaxBatch params))) time-last))
        candidates (for [ep eps]
                     (let [ws (:workspace ep)
                           may-resolve (filter (fn [hyp] (>= (get accept-cycles hyp) (:cycle ep))) rel-anomalies)]
                       {:may-resolve may-resolve :ep ep}))
        grp-candidates (group-by :may-resolve candidates)]
    (for [[may-resolve candidates] (seq grp-candidates)]
      [may-resolve (last (sort-by :cycle (map :ep candidates)))])))

(defn make-meta-hyps-order-dep
  [anomalies est _ _]
  (for [[may-resolve ep] (order-dep-candidates anomalies est)]
    (let [apriori (doubt-aggregate (new-branch-ep est ep))]
      (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
               apriori false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
               (map :contents may-resolve)
               (format "Order dependency at %s" (str ep))
               (format "Order dependency at %s" (str ep))
               {:action (partial resolve-order-dep ep)
                :resolves may-resolve
                :ep ep}))))

;;}}}

;; implausible evidence
;;{{{

(defn resolve-impl-ev
  [unrejectable est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-restored (reduce (fn [ws h]
                              (-> ws
                                  (undecide h (:cycle ep))
                                  (prevent-undecide h)
                                  (accept h nil [] [] nil {} (:cycle ep))))
                            (:workspace ep) unrejectable)
        ep-restored (assoc ep :workspace ws-restored)]
    [(update-est new-est ep-restored) params]))

(defn impl-ev-candidates
  [anomalies est time-now sensors]
  (let [ws (:workspace (cur-ep est))
        acc (accepted ws)
        ;; gather all observations rejected due to minscore
        possible-evidence (filter (fn [obs] (= :minscore (rejection-reason ws obs)))
                                  (:observation (rejected ws)))]
    ;; ask problem domain which evidence are relevant, for each anomaly
    (filter (comp not-empty :unrejectable)
            (for [anomaly anomalies]
              {:may-resolve [anomaly]
               :unrejectable ((:suggest-related-evidence-fn (:abduction @problem))
                              anomaly possible-evidence acc)}))))

(defn make-meta-hyps-implausible-evidence
  [anomalies est time-now sensors]
  (for [{:keys [unrejectable may-resolve]} (impl-ev-candidates anomalies est time-now sensors)]
    (new-hyp "ImplEv" :meta-impl-ev :meta-impl-ev
             0.0 false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
             (map :contents may-resolve)
             "Implausible evidence rejected"
             (format "Anomaly: %s\n\nSuggested related evidence:\n%s" (first may-resolve)
                     (str/join "\n" (sort-by :id unrejectable)))
             {:action (partial resolve-impl-ev unrejectable)
              :resolves may-resolve
              :unrejectable unrejectable
              :implicated unrejectable})))

;;}}}

;; make and score metahyps
;;{{{

(defn make-meta-hyps
  "Create explanations, and associated actions, for anomalies."
  [anomalies est time-now sensors]
  (let [available-meta-hyps (set (str/split (:MetaHyps params) #","))
        meta-fns (filter identity
                         [(when (available-meta-hyps "meta-impl-exp")
                            make-meta-hyps-implausible-explainers)
                          (when (available-meta-hyps "meta-order-dep")
                            make-meta-hyps-order-dep)
                          (when (available-meta-hyps "meta-impl-ev")
                            make-meta-hyps-implausible-evidence)
                          (when (available-meta-hyps "meta-conf-exp")
                            make-meta-hyps-conflicting-explainers)])]
    (doall (apply concat (for [meta-fn meta-fns] (meta-fn anomalies est time-now sensors))))))

(defn score-meta-hyp-estimate
  [meta-hyp]
  (if (= "abd-estimate" (:Metareasoning params))
    ;; this "resolves" field is really a "may resolve" field
    (assoc meta-hyp :apriori (avg (map :apriori (:resolves meta-hyp))))
    ;; not abd-estimate, so it's abd-noscores
    (assoc meta-hyp :apriori 1.0)))

(defn score-meta-hyps-estimate
  [anomalies meta-hyps est time-prev time-now sensors]
  ;; must use (doall) to avoid lazy evaluation since state/params may
  ;; change later (specifically, MinScore and Threshold change to
  ;; MetaMinScore and MetaThreshold)
  [est (doall (map score-meta-hyp-estimate meta-hyps))])

(defn score-meta-hyps-simulate-apriori
  [hyp anomalies anomalies-new resolved-cases doubt doubt-new]
  (let [apriori-diff (- (avg (map :apriori anomalies))
                        (avg (map :apriori anomalies-new)))]
    (cond (= "apriori-resolved" (:ScoreMetaHyps params))
          (avg (map :apriori resolved-cases))
          (= "apriori-diff" (:ScoreMetaHyps params))
          (max 0.0 apriori-diff)
          (= "doubt-diff" (:ScoreMetaHyps params))
          (if (<= apriori-diff 0.0) 0.0
              (max 0.0 (- doubt doubt-new)))
          ;; "doubt"
          :else
          doubt-new)))

(defn score-meta-hyps-simulate
  [anomalies meta-hyps est time-prev time-now sensors]
  (loop [est-attempted est
         hyps meta-hyps
         new-hyps []]
    (if (empty? hyps) [(goto-ep est-attempted (:id (cur-ep est))) new-hyps]
        (let [hyp (first hyps)
              [est-new params-new] ((:action hyp) est-attempted)
              result (binding [params params-new]
                       (meta-apply-and-evaluate est-attempted est-new time-now sensors))
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
                                        hyp anomalies anomalies-new resolved-cases
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
  (let [scorer (if (or (= "abd-estimate" (:Metareasoning params))
                       (= "abd-noscores" (:Metareasoning params)))
                 score-meta-hyps-estimate
                 score-meta-hyps-simulate)]
    (scorer anomalies meta-hyps est time-prev time-now sensors)))

;;}}}

;; abductive metareasoning process
;;{{{

(defn meta-abductive
  [anomalies est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps anomalies est time-now sensors)
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
               (<= (count anomalies-new) (count anomalies)))
        (recur anomalies-new
               est-applied
               (set/union attempted (set (map (fn [h] (dissoc (:contents h) :action)) meta-accepted)))
               (set/union implicated (set (map (fn [h] (:contents (:implicated h))) meta-accepted))))
        {:est-old (goto-ep est-applied (:id (cur-ep est))) :est-new est-applied}))))

;;}}}

;; fallback: false evidence
;;{{{

(defn resolve-false-ev
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

(defn assume-false-evidence
  [anomalies est time-now sensors]
  (let [[new-est _] (resolve-false-ev anomalies est)]
    (reason new-est (:time (cur-ep est)) time-now sensors :no-metareason)))

;;}}}

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [est time-prev time-now sensors]
  (let [anomalies (find-anomalies est)
        m (:Metareasoning params)
        f (cond (or (= "abd" m) (= "abd-estimate" m) (= "abd-noscores" m) (= "oracle" m))
                meta-abductive-recursive
                :else
                (constantly nil))
        result (f anomalies est time-prev time-now sensors)
        anomalies-old (if result (find-anomalies (:est-old result))
                          anomalies)
        anomalies-new (when result (find-anomalies (:est-new result)))
        [anomalies-final est-final] (cond (nil? result)
                                          [anomalies-old est]
                                          (<= (count anomalies-new) (count anomalies-old))
                                          [anomalies-new (:est-new result)]
                                          :else
                                          [anomalies-old (:est-old result)])]
    (if (:MetaRemainderIgnore params)
      (assume-false-evidence anomalies-final est-final time-now sensors)
      est-final)))


;; folded-file: t


