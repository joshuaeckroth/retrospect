(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est
          update-est goto-start-of-time print-est goto-ep ep-path goto-cycle]])
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

(defn workspace-update-sensors
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
        ws-sensors (workspace-update-sensors ws time-prev time-now sensors cycle)
        ws-explained (workspace-explain ws-sensors cycle time-now)
        est-result (est-workspace-child est ws-explained)]
    (if (or (and (:GetMoreHyps params)
                 (not= (count (:hyp-ids ws-explained))
                       (count (:hyp-ids ws))))
            (:best (:accrej ws-explained)))
      (recur est-result time-prev time-now sensors)
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
        expl-rc (set (filter (fn [h] (= :conflict (rejection-reason cur-ws h))) expl)) ;; rejected explainers due to conflict
        pc-res (sort-by :id ;; problem-cases explained by expl-rc, and therefore possibly resolved
                        (filter (fn [pc] ((set (mapcat :explains expl-rc)) (:contents pc))) problem-cases))
        acc (set (mapcat (fn [e] (filter (fn [c] (accepted? cur-ws c)) ;; acc that conflict with expl-rc
                                   (find-conflicts cur-ws e))) expl-rc))
        inner-hyps (set (map :id (mapcat :hyps acc))) ;; inner hyps, if any, of acc
        acc-no-inner (sort-by :id (filter #(not (inner-hyps (:id %))) acc)) ;; keep only those that are not inner hyps
        acc-no-inner-ids (set (map :id acc-no-inner))
        ;; don't do any batching
        ep-rejs (filter (fn [ep] (and (= time-now (:time ep))
                                (some acc-no-inner-ids (:acc (:accrej (:workspace ep))))))
                   (ep-path est))
        rejs-deltas (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                (:cycle ep)
                                (get-in ep [:workspace :accrej :best])])
                       ep-rejs)
        ;; this will sort by delta first, then cycle
        bad-bests (reverse (sort (set (filter (fn [[delta _ best]] (and delta best)) rejs-deltas))))]
    (for [[delta cycle hyp] bad-bests]
      {:implicated hyp :cycle cycle :rejected expl-rc :delta delta :may-resolve pc-res})))

(defn meta-rej-conflict
  [problem-cases est-orig time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est-orig
         est est-orig
         tried-implicated #{}]
    (let [ ;; take the earliest rej-conflict
          {:keys [implicated]} (last (sort-by :cycle (find-rej-conflict-candidates
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
        expl-rejected-minscore (sort-by :id (filter (fn [h] (= :minscore (rejection-reason cur-ws h))) expl))
        relevant-problem-cases (sort-by :id (filter (fn [pc] (some #{(:contents pc)}
                                                          (mapcat :explains expl-rejected-minscore)))
                                               problem-cases))]
    {:implicated expl-rejected-minscore
     :may-resolve relevant-problem-cases}))

(defn meta-lower-minscore
  [problem-cases est-orig time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est-orig
         est est-orig
         implicated-before #{}]
    (let [cur-ws (:workspace (cur-ep est))
          {:keys [implicated]} (find-rej-minscore-candidates problem-cases est time-now)
          implicated-untried (filter #(not (implicated-before (:contents %))) implicated)]
      (if (not-empty implicated-untried)
        (let [[est-action params-action] (action-prevent-rejection-minscore implicated-untried est)
              {:keys [est-old est-new]} (binding [params params-action]
                                          (meta-apply-and-evaluate est est-action time-now sensors))
              problem-cases-new (find-problem-cases est-new)]
          (if (not-empty problem-cases-new)
            (recur problem-cases-new
                   est-old
                   est-new
                   (set/union implicated-before (set (map :contents implicated-untried))))
            {:est-old est-old :est-new est-new}))
        {:est-old est-prior :est-new est}))))

(defn meta-hyp-conflicts?
  [hyp1 hyp2]
  ;; all meta-hyp types conflict; only one can be accepted
  (and (not= hyp1 hyp2)
       ((:meta-hyp-types @reasoner) (:type hyp1))
       ((:meta-hyp-types @reasoner) (:type hyp2))))

(defn make-meta-hyps-order-dep
  [problem-cases est time-prev time-now available-meta-hyps]
  (if (not (available-meta-hyps "meta-order-dep")) []
      ;; order dependency among the observations; a no-expl-offered situation
      (if (and (not= 0 time-prev)
               ;; require that some problem case has no known explainers
               (some (fn [pc] (empty? (explainers (:workspace (cur-ep est)) pc))) problem-cases))
        (let [batchbeg-ep (cur-ep (goto-start-of-time est 0))
              batchbeg-hyp (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
                                    0.1 false meta-hyp-conflicts?
                                    (map :contents problem-cases)
                                    (format "Order dependency at time 0, ep %s" (str batchbeg-ep))
                                    (format "Order dependency at time 0, ep %s" (str batchbeg-ep))
                                    {:action (partial action-batch batchbeg-ep)
                                     :cycle (:cycle batchbeg-ep)
                                     :time 0
                                     :time-delta time-now})
              batch1-ep (cur-ep (goto-start-of-time est (dec time-now)))
              batch1-hyp (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
                                  0.1 false meta-hyp-conflicts?
                                  (map :contents problem-cases)
                                  (format "Order dependency at time %d, ep %s"
                                     (dec time-now) (str batch1-ep))
                                  (format "Order dependency at time %d, ep %s"
                                     (dec time-now) (str batch1-ep))
                                  {:action (partial action-batch batch1-ep)
                                   :cycle (:cycle batch1-ep)
                                   :time (dec time-now)
                                   :time-delta 1})]
          ;; don't allow batching more than once (accumulating batches)
          (if (= (:time batch1-ep) (dec time-now))
            [batchbeg-hyp batch1-hyp]
            [batchbeg-hyp]))
        ;; time-prev == 0, so this is a "static" case or we have not
        ;; done much reasoning yet
        [])))

(defn make-meta-hyps-rej-conflict
  [problem-cases est time-prev time-now available-meta-hyps]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (if (not (available-meta-hyps "meta-rej-conflict")) []
      (for [{:keys [implicated cycle rejected delta may-resolve]}
            (find-rej-conflict-candidates problem-cases est time-now)]
        (new-hyp "RejConflict" :meta-rej-conflict :meta-rej-conflict
                 0.5 false meta-hyp-conflicts?
                 (map :contents may-resolve)
                 (format "%s rejected some explainers" implicated)
                 (format "%s rejected these explainers (cycle %d, delta %.2f):\n%s"
                    (str implicated) cycle delta (str/join "\n" (map str rejected)))
                 {:action (partial action-preemptively-reject [implicated])
                  :cycle cycle
                  :delta delta
                  :implicated [implicated]}))))

(defn make-meta-hyps-rej-minscore
  [problem-cases est time-prev time-now available-meta-hyps]
  ;; were some explainers omitted due to high min-score?
  (if (not (available-meta-hyps "meta-rej-minscore")) []
      (let [minscore (/ (double (:MinScore params)) 100.0)
            {:keys [implicated may-resolve]} (find-rej-minscore-candidates problem-cases est time-now)]
        (if (not-empty implicated)
          [(new-hyp "TooHighMinScore" :meta-rej-minscore :meta-rej-minscore
                    0.25 false meta-hyp-conflicts?
                    (map :contents may-resolve)
                    "Explainers rejected due to too-high min-score"
                    (format "These explainers were rejected due to too-high min-score:\n%s\n\nRelevant problem cases:\n%s"
                       (str/join "\n" (sort (map str implicated)))
                       (str/join "\n" (sort (map str may-resolve))))
                    {:action (partial action-prevent-rejection-minscore implicated)
                     :implicated implicated
                     :min-score-delta (- minscore (apply min (map :apriori implicated)))
                     :max-score-delta (- minscore (apply max (map :apriori implicated)))
                     :avg-score-delta (- minscore (avg (map :apriori implicated)))})]
          []))))

(defn make-meta-hyps
  "Create explanations, and associated actions, for problem-cases."
  [problem-cases est time-prev time-now]
  (let [available-meta-hyps (set (str/split (:MetaHyps params) #","))]
    (concat
     (make-meta-hyps-order-dep problem-cases est time-prev time-now available-meta-hyps)
     (make-meta-hyps-rej-conflict problem-cases est time-prev time-now available-meta-hyps)
     (make-meta-hyps-rej-minscore problem-cases est time-prev time-now available-meta-hyps))))

(defn score-meta-hyps-estimate
  [problem-cases meta-hyps est time-prev time-now sensors]
  ;; new-meta-hyps are scored meta-rej-conflicts
  (let [new-meta-hyps (for [h meta-hyps]
                        (cond (= :meta-rej-conflict (:type h))
                              (assoc h :apriori (- 1.0 (:delta h)))
                              :else h))]
    [est new-meta-hyps]))

(defn score-meta-hyps-simulate-apriori
  [hyp problem-cases problem-cases-new doubt doubt-new]
  (if (<= 0.0 (- doubt-new doubt))
    ;; doubt-diff is positive (the meta-hyp increases doubt);
    ;; forget it
    0.0
    ;; otherwise, doubt-diff is negative (the meta-hyp decreases doubt)
    (if (and (:ComplexMetaRejMinscoreScoring params)
             (= :meta-rej-minscore (:type hyp)))
      ;; different scoring for rej-minscore meta-hyps
      (max 0.0
           (- (* 0.5 (- 1.0
                        (max 0.0
                             (- (avg (map :apriori problem-cases))
                                (avg (map :apriori problem-cases-new))))))
              (* 2.0 (:max-score-delta hyp))))
      ;; normal scoring (non-rej-minscore meta-hyps)
      (cond (= "diff" (:ScoreMetaHyps params))
            (max 0.0
                 (- (avg (map :apriori problem-cases))
                    (avg (map :apriori problem-cases-new))
                    (if (nil? (:penalty hyp)) 0.0
                        (:penalty hyp))))
            ;; "doubt"
            :else
            doubt-new))))

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
  (if (:EstimateMetaScores params)
    (let [[est-new meta-hyps-new]
          (score-meta-hyps-simulate
           problem-cases meta-hyps est time-prev time-now sensors)]
      (score-meta-hyps-estimate
       problem-cases meta-hyps-new est-new time-prev time-now sensors))
    (score-meta-hyps-simulate
     problem-cases meta-hyps est time-prev time-now sensors)))

(defn meta-abductive
  [problem-cases est time-prev time-now sensors]
  (let [meta-params (assoc params
                      :MinScore (:MetaMinScore params)
                      :Threshold (:MetaThreshold params)
                      :GetMoreHyps false)
        meta-hyps (make-meta-hyps problem-cases est time-prev time-now)
        [est-new meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps
                                                    est time-prev time-now sensors)
        meta-hyps-scored-explanatory (filter #(not-empty (:explains %)) meta-hyps-scored)
        meta-est (new-child-ep
                  (init-est (assoc (init-workspace)
                              :meta-oracle (:meta-oracle (:workspace (cur-ep est))))))
        meta-ws (binding [params meta-params]
                  (reduce #(add %1 %2 0)
                     (reduce #(add-observation %1 %2 0)
                        (:workspace (cur-ep meta-est)) problem-cases)
                     meta-hyps-scored-explanatory))
        meta-est-reasoned (binding [params meta-params]
                            (reason (update-est meta-est (assoc (cur-ep meta-est)
                                                           :workspace meta-ws))
                                    0 1 nil :no-metareason))
        meta-ws-reasoned (:workspace (cur-ep meta-est-reasoned))
        ;; take out the "observations"
        meta-accepted (filter (fn [hyp] (not ((set (map :contents problem-cases)) (:contents hyp))))
                         (:all (accepted meta-ws-reasoned)))
        est-new-meta-est (update-est est-new (assoc (cur-ep est)
                                               :meta-est meta-est-reasoned))
        meta-hyp-compare (fn [h1 h2] (if (= 0 (compare (:apriori h2) (:apriori h1)))
                                      (cond (= :meta-order-dep (:type h1)) -1
                                            (= :meta-order-dep (:type h2)) 1
                                            (= :meta-rej-conflict (:type h1)) -1
                                            (= :meta-rej-conflict (:type h2)) -1
                                            :else (compare (:id h1) (:id h2)))
                                      (compare (:apriori h2) (:apriori h1))))
        best (first (sort meta-hyp-compare meta-accepted))]
    (if (:EstimateMetaScores params)
      (if best
        ;; the action has not been done yet, so do it
        (let [[est-new params-new] ((:action best) est-new-meta-est)
              result (binding [params params-new]
                       (meta-apply-and-evaluate
                        est-new-meta-est est-new time-now sensors))]
          {:est-old (:est-old result)
           :est-new (:est-new result)
           :best best})
        {:est-old (goto-ep est-new-meta-est (:id (cur-ep est)))
         :est-new est-new-meta-est})
      {:est-old (goto-ep est-new-meta-est (:id (cur-ep est)))
       :est-new (if (nil? best) est-new-meta-est
                    (goto-ep est-new-meta-est (:final-ep-id best)))
       :best best})))

(defn meta-abductive-recursive
  [problem-cases est time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est est
         attempted #{}
         implicated #{}]
    (let [{:keys [est-old est-new best]} (meta-abductive problem-cases est time-prev time-now sensors)
          problem-cases-new (find-problem-cases est-new)]
      (if (and best
               (not (attempted (dissoc (:contents best) :action)))
               (or (nil? (:implicated best))
                   (not-every? implicated (map :contents (:implicated best))))
               (not-empty problem-cases-new))
        (recur problem-cases-new
               est-new
               (conj attempted (dissoc (:contents best) :action))
               (if (:implicated best)
                 (set/union implicated (set (map :contents (:implicated best))))
                 implicated))
        {:est-old est-old :est-new est-new}))))

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
