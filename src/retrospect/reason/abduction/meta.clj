(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.math.combinatorics :only [combinations]])
  (:use [retrospect.epistemicstates])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.evaluate :only [avg]])
  (:use [retrospect.reason.abduction.evaluate :only [doubt-aggregate]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(declare metareason)

(defn find-problem-cases
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (set (no-explainers workspace))))

(defn metareasoning-activated?
  [est]
  (and (not= "none" (:Metareasoning params))
       (not-empty (find-problem-cases est))))

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

(defn meta-batchbeg
  [problem-caes est time-prev time-now sensors]
  (when (not= time-prev 0)
    (let [batchbeg-ep (cur-ep (goto-start-of-time est 0))
          new-est (new-branch-ep est batchbeg-ep)]
      (meta-apply-and-evaluate est new-est time-now sensors))))

(defn meta-batch1
  [problem-caes est time-prev time-now sensors]
  (when (not= time-prev 0)
    (let [batch1-ep (cur-ep (goto-start-of-time est (dec time-now)))]
      (when (= (:time batch1-ep) (dec time-now))
        (let [new-est (new-branch-ep est batch1-ep)]
          (meta-apply-and-evaluate est new-est time-now sensors))))))

(defn action-batch
  [ep est]
  [(new-branch-ep est ep) params])

(defn action-prevent-rejection-minscore
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-prevent-rejection (reduce (fn [ws hyp]
                                  ;; undeciding does not affect prevent rejection tags
                                  (-> ws (undecide hyp)
                                     (prevent-rejection hyp :minscore)))
                                (:workspace ep) implicated)
        ep-prevent-rejection (assoc ep :workspace ws-prevent-rejection)]
    [(update-est new-est ep-prevent-rejection) params]))

(defn action-ignore
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-ignored (reduce (fn [ws hyp]
                        (-> ws (undecide hyp)
                           (reject hyp :ignoring (:cycle ep))))
                      (:workspace ep) implicated)
        ep-ignored (assoc ep :workspace ws-ignored)]
    [(update-est new-est ep-ignored) params]))

(defn action-undecide
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-undecided (reduce undecide (:workspace ep) implicated)
        ep-undecided (assoc ep :workspace ws-undecided)]
    [(update-est new-est ep-undecided) params]))

(defn action-preemptively-reject
  [implicated est]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws-rejected (reduce (fn [ws hyp]
                         (-> ws (undecide hyp)
                            (reject hyp :preemptive (:cycle ep))))
                       (:workspace ep) implicated)
        ep-rejected (assoc ep :workspace ws-rejected)]
    [(update-est new-est ep-rejected) params]))

(defn find-rej-conflict-candidates
  [problem-cases est time-now]
  (let [cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) problem-cases)) ;; explainers of problem-cases
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
        earliest-rejs-deltas (for [hyp (map :hyp ep-rejs-deltas)]
                               (first (sort-by :cycle (filter #(= hyp (:hyp %)) ep-rejs-deltas))))]
    (set (filter #(not-empty (:may-resolve %))
            (for [{:keys [delta cycle hyp]} earliest-rejs-deltas]
              (let [expl-conf (filter #(conflicts? hyp %) expl-rc)
                    expl-explained (set (mapcat :explains expl-conf))
                    ;; problem-cases explained by expl-conf, and therefore possibly resolved
                    pc-res (filter (fn [pc] (expl-explained (:contents pc))) problem-cases)]
                {:implicated hyp :cycle cycle :delta delta
                 :expl-conflicting (sort-by :id expl-conf) :may-resolve (sort-by :id pc-res)}))))))

(defn meta-rej-conflict
  [problem-cases est-orig time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est-orig
         est est-orig
         tried-implicated #{}]
    (let [ ;; take the lowest delta
          {:keys [implicated]} (first (sort-by :delta (find-rej-conflict-candidates
                                                       problem-cases est time-now)))]
      (if (and implicated (not (tried-implicated implicated)))
        (let [[est-action params-action] (action-preemptively-reject [implicated] est)
              {:keys [est-old est-new]} (binding [params params-action]
                                          (meta-apply-and-evaluate est est-action time-now sensors))
              problem-cases-new (find-problem-cases est-new)]
          (if (not-empty problem-cases-new)
            (recur problem-cases-new est-old est-new (conj tried-implicated implicated))
            {:est-old est-old :est-new est-new}))
        {:est-old est-prior :est-new est}))))

(defn find-rej-minscore-candidates
  [problem-cases est time-now]
  (let [cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) problem-cases)) ;; explainers of problem-cases
        expl-rejected-minscore (sort-by :id (filter (fn [h] (= :minscore (rejection-reason cur-ws h))) expl))]
    (set (filter #(not-empty (:may-resolve %))
            (for [e expl-rejected-minscore]
              {:implicated e
               :may-resolve (sort-by :id (filter (fn [pc] (some #{(:contents pc)} (:explains e))) problem-cases))})))))

(defn meta-lower-minscore
  [problem-cases est-orig time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est-orig
         est est-orig
         implicated-before #{}]
    (let [cur-ws (:workspace (cur-ep est))
          ;; take the greatest scoring hyp to prevent-rejection
          {:keys [implicated]} (last (sort-by (comp :apriori :implicated)
                                              (find-rej-minscore-candidates problem-cases est time-now)))]
      (if (and implicated (not (implicated-before (:contents implicated))))
        (let [[est-action params-action] (action-prevent-rejection-minscore [implicated] est)
              {:keys [est-old est-new]} (binding [params params-action]
                                          (meta-apply-and-evaluate est est-action time-now sensors))
              problem-cases-new (find-problem-cases est-new)]
          (if (not-empty problem-cases-new)
            (recur problem-cases-new est-old est-new (conj implicated-before (:contents implicated)))
            {:est-old est-old :est-new est-new}))
        {:est-old est-prior :est-new est}))))

(defn find-order-dep-candidates
  [problem-cases est time-prev time-now]
  ;; order dependency among the observations; a no-expl-offered situation
  (let [no-expl-offered (filter (fn [pc] (empty? (explainers (:workspace (cur-ep est)) pc))) problem-cases)]
    ;; require that some problem case has no known explainers (no-expl-offered)
    (if (and (not= 0 time-prev) (not-empty no-expl-offered))
      (let [batchbeg {:ep (cur-ep (goto-start-of-time est 0))
                      :may-resolve no-expl-offered}
            batch1 {:ep (cur-ep (goto-start-of-time est (dec time-now)))
                    :may-resolve no-expl-offered}]
        ;; don't allow batching more than once (accumulating batches)
        (if (= (:time (:ep batch1)) (dec time-now))
          [batchbeg batch1]
          [batchbeg]))
      ;; time-prev == 0, so this is a "static" case or we have not
      ;; done much reasoning yet; or there are no no-expl-offered cases
      [])))

(defn meta-hyp-conflicts?
  [hyp1 hyp2]
  ;; all meta-hyp types conflict; only one can be accepted
  (and (not= hyp1 hyp2)
       ((:meta-hyp-types @reasoner) (:type hyp1))
       ((:meta-hyp-types @reasoner) (:type hyp2))))

(defn make-meta-hyps-order-dep
  [problem-cases est time-prev time-now available-meta-hyps]
  (if (not (available-meta-hyps "meta-order-dep")) []
      (let [[batchbeg batch1] (find-order-dep-candidates problem-cases est time-prev time-now)
            batchbeg-hyp (when batchbeg
                           (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
                                    0.0 false meta-hyp-conflicts?
                                    (map :contents (:may-resolve batchbeg))
                                    (format "Order dependency at time 0, ep %s" (str (:ep batchbeg)))
                                    (format "Order dependency at time 0, ep %s" (str (:ep batchbeg)))
                                    {:action (partial action-batch (:ep batchbeg))
                                     :resolves (:may-resolve batchbeg)
                                     :cycle (:cycle (:ep batchbeg))
                                     :time 0
                                     :time-delta time-now}))
            batch1-hyp (when batch1
                         (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
                                  0.0 false meta-hyp-conflicts?
                                  (map :contents (:may-resolve batch1))
                                  (format "Order dependency at time %d, ep %s"
                                     (dec time-now) (str (:ep batch1)))
                                  (format "Order dependency at time %d, ep %s"
                                     (dec time-now) (str (:ep batch1)))
                                  {:action (partial action-batch (:ep batch1))
                                   :resolves (:may-resolve batch1)
                                   :cycle (:cycle (:ep batch1))
                                   :time (dec time-now)
                                   :time-delta 1}))]
        (filter identity [batchbeg-hyp batch1-hyp]))))

(defn make-meta-hyps-rej-conflict
  [problem-cases est time-prev time-now available-meta-hyps]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (if (not (available-meta-hyps "meta-rej-conflict")) []
      (for [{:keys [implicated cycle rejected delta may-resolve]}
            (find-rej-conflict-candidates problem-cases est time-now)]
        (new-hyp "RejConflict" :meta-rej-conflict :meta-rej-conflict
                 0.0 false meta-hyp-conflicts?
                 (map :contents may-resolve)
                 (format "%s rejected some explainers" implicated)
                 (format "%s rejected these explainers (cycle %d, delta %.2f):\n%s"
                    (str implicated) cycle delta (str/join "\n" (map str rejected)))
                 {:action (partial action-preemptively-reject [implicated])
                  :resolves may-resolve
                  :cycle cycle
                  :delta delta
                  :implicated implicated}))))

(defn make-meta-hyps-rej-minscore
  [problem-cases est time-prev time-now available-meta-hyps]
  ;; were some explainers omitted due to high min-score?
  (if (not (available-meta-hyps "meta-rej-minscore")) []
      (for [{:keys [implicated may-resolve]} (find-rej-minscore-candidates problem-cases est time-now)]
        (new-hyp "TooHighMinScore" :meta-rej-minscore :meta-rej-minscore
                 0.0 false meta-hyp-conflicts?
                 (map :contents may-resolve)
                 "Explainer rejected due to too-high min-score"
                 (format "This explainer was rejected due to too-high min-score: %s\n\nRelevant problem cases:\n%s"
                    (str implicated)
                    (str/join "\n" (sort (map str may-resolve))))
                 {:action (partial action-prevent-rejection-minscore [implicated])
                  :resolves may-resolve
                  :implicated implicated
                  :score-delta (- (/ (double (:MinScore params)) 100.0) (:apriori implicated))}))))

(defn make-meta-hyps
  "Create explanations, and associated actions, for problem-cases."
  [problem-cases est time-prev time-now]
  (let [available-meta-hyps (set (str/split (:MetaHyps params) #","))]
    (apply concat
           (for [meta-fn [make-meta-hyps-order-dep
                          make-meta-hyps-rej-conflict
                          make-meta-hyps-rej-minscore]]
             (meta-fn problem-cases est time-prev time-now available-meta-hyps)))))

(defn score-meta-hyps-estimate
  [problem-cases meta-hyps est time-prev time-now sensors]
  ;; must use (doall) to avoid lazy evaluation since state/params may
  ;; change later (specifically, MinScore and Threshold change to
  ;; MetaMinScore and MetaThreshold)
  [est (doall (for [h meta-hyps]
                (assoc h :apriori
                       (cond (= :meta-order-dep (:type h))
                             1.0
                             (= :meta-rej-conflict (:type h))
                             (- 1.0 (:delta h))
                             (= :meta-rej-minscore (:type h))
                             (:score-delta h)
                             :else ;; some non-meta-hyp type
                             (:apriori h)))))])

(defn score-meta-hyps-simulate-apriori
  [hyp problem-cases problem-cases-new doubt doubt-new]
  (if (<= 0.0 (- doubt-new doubt))
    ;; doubt-diff is positive (the meta-hyp increases doubt);
    ;; forget it
    0.0
    ;; otherwise, doubt-diff is negative (the meta-hyp decreases doubt)
    (cond (= "apriori-diff" (:ScoreMetaHyps params))
          (max 0.0
               (- (avg (map :apriori problem-cases))
                  (avg (map :apriori problem-cases-new))
                  (if (nil? (:penalty hyp)) 0.0
                      (:penalty hyp))))
          (= "doubt-diff" (:ScoreMetaHyps params))
          (- doubt doubt-new) ;; known to be non-negative due to (if) above
          ;; "doubt"
          :else
          doubt-new)))

(defn score-meta-hyps-simulate
  [problem-cases meta-hyps est time-prev time-now sensors]
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
              problem-cases-new (find-problem-cases (:est-new result))
              resolved-cases (filter (fn [pc] (not ((set (map :contents problem-cases-new))
                                              (:contents pc))))
                                problem-cases)]
          (recur (:est-old result) (rest hyps)
                 (conj new-hyps
                       (assoc hyp :explains (map :contents resolved-cases)
                              :resolves resolved-cases
                              :problem-cases-prior problem-cases
                              :problem-cases-after problem-cases-new
                              :final-ep-id (:id (cur-ep (:est-new result)))
                              :apriori (score-meta-hyps-simulate-apriori
                                        hyp problem-cases problem-cases-new
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
                                       (str/join "\n" (sort-by :id problem-cases))
                                       (str/join "\n" (sort-by :id problem-cases-new))
                                       doubt
                                       doubt-new
                                       (avg (map :apriori problem-cases))
                                       (avg (map :apriori problem-cases-new))
                                       (- (avg (map :apriori problem-cases))
                                          (avg (map :apriori problem-cases-new)))))))))))

(defn score-meta-hyps
  [problem-cases meta-hyps est time-prev time-now sensors]
  (let [scorer (if (:EstimateMetaScores params)
                 score-meta-hyps-estimate
                 score-meta-hyps-simulate)]
    (scorer problem-cases meta-hyps est time-prev time-now sensors)))

(defn meta-abductive
  [problem-cases est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps problem-cases est time-prev time-now)
        [est-new meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps est time-prev time-now sensors)
        meta-est (new-child-ep (init-est (assoc (init-workspace)
                                           :meta-oracle (:meta-oracle (:workspace (cur-ep est))))))
        meta-params (assoc params
                      :MinScore (:MetaMinScore params)
                      :Threshold (:MetaThreshold params)
                      :GetMoreHyps false)
        meta-ws (binding [params meta-params]
                  (let [ws-obs (reduce (fn [ws h] (add-observation ws h 0))
                                  (:workspace (cur-ep meta-est)) problem-cases)]
                    (reduce (fn [ws h] (add ws h 0)) ws-obs meta-hyps-scored)))
        meta-est-reasoned (binding [params meta-params]
                            (reason (update-est-ep meta-est :workspace meta-ws) 0 1 nil :no-metareason))]
    (update-est-ep est-new :meta-est meta-est-reasoned)))

(defn meta-abductive-recursive
  [problem-cases est time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est est
         attempted #{}
         implicated #{}]
    (let [est-abd (meta-abductive problem-cases est time-prev time-now sensors)
          meta-workspace (:workspace (cur-ep (:meta-est (cur-ep est-abd))))
          meta-accepted (filter (fn [h] (and (not (attempted (dissoc (:contents h) :action)))
                                       (not (implicated (:contents (:implicated h))))))
                           (apply concat (vals (select-keys (accepted meta-workspace)
                                                            (:meta-hyp-types @reasoner)))))
          est-applied (if (empty? meta-accepted) est-abd
                          (reduce (fn [est hyp]
                               (let [[est-new params-new] ((:action hyp) est)]
                                 (binding [params params-new]
                                   (:est-new (meta-apply-and-evaluate est est-new time-now sensors)))))
                             est-abd meta-accepted))
          problem-cases-new (when (not-empty meta-accepted) (find-problem-cases est-applied))]
      (if (and (not-empty meta-accepted) (not-empty problem-cases-new))
        (recur problem-cases-new
               est-applied
               (set/union attempted (set (map (fn [h] (dissoc (:contents h) :action)) meta-accepted)))
               (set/union implicated (set (map (fn [h] (:contents (:implicated h))) meta-accepted))))
        {:est-old (goto-ep est-applied (:id (cur-ep est))) :est-new est-applied}))))

(defn resolve-by-ignoring
  [problem-cases est time-prev time-now sensors]
  (let [[new-est _] (action-ignore problem-cases est)]
    (reason new-est time-prev time-now sensors :no-metareason)))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [est time-prev time-now sensors]
  (let [problem-cases (find-problem-cases est)
        m (:Metareasoning params)
        f (cond (= "batch1" m)
                meta-batch1
                (= "batchbeg" m)
                meta-batchbeg
                (= "lower-minscore" m)
                meta-lower-minscore
                (= "rej-conflict" m)
                meta-rej-conflict
                (= "abd" m)
                meta-abductive-recursive
                (= "ignore" m)
                (constantly nil))
        result (f problem-cases est time-prev time-now sensors)
        problem-cases-old (if result (find-problem-cases (:est-old result))
                              problem-cases)
        problem-cases-new (when result (find-problem-cases (:est-new result)))]
    (cond (nil? result)
          (resolve-by-ignoring problem-cases-old est time-prev time-now sensors)
          (empty? problem-cases-new)
          (:est-new result)
          (< (count problem-cases-new) (count problem-cases-old))
          (resolve-by-ignoring problem-cases-new (:est-new result) time-prev time-now sensors)
          :else
          (resolve-by-ignoring problem-cases-old (:est-old result) time-prev time-now sensors))))
