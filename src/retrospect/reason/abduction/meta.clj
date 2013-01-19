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
    (set (map #(lookup-hyp workspace %) (get-no-explainers workspace)))))

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
    (let [ws (explain workspace cycle time-now)]
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

(defn meta-batch1
  [problem-caes est time-prev time-now sensors]
  (when (not= time-prev 0)
    (let [batch1-ep (cur-ep (goto-start-of-time est (dec time-now)))]
      (when (= (:time batch1-ep) (dec time-now))
        (let [new-est (new-branch-ep est batch1-ep)]
          (meta-apply-and-evaluate est new-est time-now sensors))))))

(defn meta-batch-weakest
  [problem-cases est _ time-now sensors]
  (let [doubts-cycles (reverse (sort-by first
                                        (filter first
                                           (map (fn [ep]
                                                [(calc-doubt (:workspace ep)) (:cycle ep)])
                                              (ep-path est)))))
        highest-doubt (ffirst doubts-cycles)
        prior-cycle (dec (second (first (sort-by second (filter #(= highest-doubt (first %))
                                                           doubts-cycles)))))
        new-est (new-branch-ep est (cur-ep (goto-cycle est prior-cycle)))]
    (meta-apply-and-evaluate est new-est time-now sensors)))

(defn action-batch
  [ep est]
  [(new-branch-ep est ep)
   params])

(defn action-lower-minscore
  [new-minscore time-now est]
  [(new-branch-ep est (cur-ep (goto-start-of-time est time-now)))
   (assoc params :MinScore new-minscore)])

(defn action-ignore
  [hyp est]
  (let [ws-old (:workspace (cur-ep est))
        new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-ignored (reject-many (:workspace new-ep) [hyp] :ignoring (:cycle new-ep))]
    [(update-est new-est (assoc new-ep :workspace ws-ignored))
     params]))

(defn action-preemptively-reject
  [hyp cycle est]
  (let [new-est (new-branch-ep est (cur-ep (goto-cycle est (- cycle 2))))
        ep (cur-ep new-est)
        ws-rejected (reject-many (:workspace ep) [hyp] :preemptive (:cycle ep))
        ep-rejected (assoc ep :workspace ws-rejected)]
    [(update-est new-est ep-rejected)
     params]))

(defn find-rej-conflict-candidates
  [problem-cases est time-now cur-ws expl]
  (let [expl-rejected-conflicts
        (sort-by :id (filter (fn [h] (= :conflict (rejection-reason cur-ws h)))
                        expl))
        problem-cases-possibly-resolved
        (sort-by :id (filter (fn [pc] ((set (mapcat :explains expl-rejected-conflicts))
                                 (:contents pc)))
                        problem-cases))
        acc-conflicting (set (mapcat
                              (fn [e] (filter (fn [c] (accepted-before? cur-ws c e))
                                        (find-conflicts-all cur-ws e)))
                              expl-rejected-conflicts))
        inner-hyps (set (map :id (mapcat :hyps acc-conflicting)))
        acc-conflicting-no-comps (sort-by :id (filter #(not (inner-hyps (:id %)))
                                                 acc-conflicting))
        ;; which ep-states rejected these expl (essentials are
        ;; allowed; we may still want to reject an "essential"
        ;; explainer;
        ;; also filter out any ep's that do a batch
        ep-rejs (filter #(= time-now (:time %))
                   (filter (fn [ep] (some (set (map :id acc-conflicting-no-comps))
                                 (:acc (:accrej (:workspace ep)))))
                      (ep-path est)))
        rejs-deltas (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                (:cycle ep)
                                (get-in ep [:workspace :accrej :best])])
                       ep-rejs)
        ;; this will sort by delta first, then cycle
        bad-bests (reverse (sort (set (filter (fn [[delta _ best]] (and delta best))
                                         rejs-deltas))))]
    (for [[delta cycle hyp] bad-bests]
      {:implicated hyp
       :cycle cycle
       :rejected expl-rejected-conflicts
       :delta delta
       :may-resolve problem-cases-possibly-resolved})))

(defn meta-rej-conflict
  [problem-cases est-orig time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est-orig
         est est-orig
         tried-implicated #{}]
    (let [cur-ws (:workspace (cur-ep est))
          expl (set (mapcat #(explainers cur-ws %) problem-cases))
          ;; take the earliest rej-conflict
          {:keys [implicated cycle]}
          (last (sort-by :cycle (find-rej-conflict-candidates
                                 problem-cases est time-now cur-ws expl)))]
      (if (and implicated (not (tried-implicated implicated)))
        (let [[est-action params-action]
              (action-preemptively-reject implicated cycle est)
              {:keys [est-old est-new]}
              (binding [params params-action]
                (meta-apply-and-evaluate est est-action time-now sensors))
              problem-cases-new (find-problem-cases est-new)]
          (if (not-empty problem-cases-new)
            (recur problem-cases-new
                   est-old
                   est-new
                   (conj tried-implicated implicated))
            {:est-old est-old :est-new est-new}))
        {:est-old est-prior :est-new est}))))

(defn find-rej-minscore-candidates
  [problem-cases cur-ws expl]
  (let [minscore (/ (double (:MinScore params)) 100.0)
        expl-rejected-minscore
        (sort-by :id (filter (fn [h] (= :minscore (rejection-reason cur-ws h)))
                        expl))
        relevant-problem-cases
        (sort-by :id (filter (fn [pc] (some #{(:contents pc)}
                                   (mapcat :explains expl-rejected-minscore)))
                        problem-cases))]
    {:implicated expl-rejected-minscore
     :may-resolve relevant-problem-cases}))

(defn meta-lower-minscore
  [problem-cases est-orig time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est-orig
         est est-orig]
    (let [cur-ws (:workspace (cur-ep est))
          expl (set (mapcat #(explainers cur-ws %) problem-cases))
          {:keys [implicated may-resolve]}
          (find-rej-minscore-candidates problem-cases cur-ws expl)]
      (if (not-empty implicated)
        (let [new-minscore (* 100.0 (- (apply min (map :apriori implicated)) 0.01))
              [est-action params-action] (action-lower-minscore
                                          new-minscore time-now est)
              {:keys [est-old est-new]}
              (binding [params params-action]
                (meta-apply-and-evaluate est est-action time-now sensors))
              problem-cases-new (find-problem-cases est-new)]
          (if (not-empty problem-cases-new)
            (recur problem-cases-new
                   est-old
                   est-new)
            {:est-old est-old :est-new est-new}))
        {:est-old est-prior :est-new est}))))

(defn meta-hyp-conflicts?
  [hyp1 hyp2]
  (and (not= hyp1 hyp2)
       ((:meta-hyp-types @reasoner) (:type hyp1))
       ((:meta-hyp-types @reasoner) (:type hyp2))))

(defn make-meta-hyps-order-dep
  [problem-cases est time-prev time-now available-meta-hyps cur-ws expl]
  (if (not (available-meta-hyps "meta-order-dep")) []
      ;; order dependency among the observations; a no-expl-offered situation
      (if (not= 0 time-prev)
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
            [batch1-hyp]
            []))
        ;; time-prev == 0, so this is a "static" case or we have not
        ;; done much reasoning yet
        [])))

(comment
  (loop [hs (sort-by :id (filter #(empty? (explainers cur-ws %)) problem-cases))
         order-dep-hyps [batchbeg-hyp batch1-hyp]]
    (if (empty? hs) order-dep-hyps
        (let [t (:time (cur-ep (goto-cycle est (accepted-cycle cur-ws (first hs)))))
              ep (cur-ep (goto-start-of-time est (dec t)))]
          ;; don't offer two order-dep hyps that go back to the same time
          (if (some #(= (dec t) (:time %)) order-dep-hyps)
            (recur (rest hs) order-dep-hyps)
            (recur (rest hs)
                   (conj
                    order-dep-hyps
                    (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
                             0.1 false meta-hyp-conflicts?
                             (:contents (first hs))
                             (format "Order dependency at time %d, ep %s" (dec t) (str ep))
                             (format "Order dependency at time %d, ep %s" (dec t) (str ep))
                             {:action (partial action-batch ep)
                              :cycle (:cycle ep)
                              :time (dec t)
                              :time-delta (- time-now (dec t))}))))))))

(defn make-meta-hyps-rej-conflict
  [problem-cases est time-prev time-now available-meta-hyps cur-ws expl]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (if (not (available-meta-hyps "meta-rej-conflict")) []
      (for [{:keys [implicated cycle rejected delta may-resolve]}
            (find-rej-conflict-candidates problem-cases est time-now cur-ws expl)]
        (new-hyp "RejConflict" :meta-rej-conflict :meta-rej-conflict
                 0.5 false meta-hyp-conflicts?
                 (map :contents may-resolve)
                 (format "%s rejected some explainers" implicated)
                 (format "%s rejected these explainers (cycle %d, delta %.2f):\n%s"
                    (str implicated) cycle delta (str/join "\n" (map str rejected)))
                 {:action (partial action-preemptively-reject implicated cycle)
                  :cycle cycle
                  :implicated [implicated]}))))

(defn make-meta-hyps-rej-minscore
  [problem-cases est time-prev time-now available-meta-hyps cur-ws expl]
  ;; were some explainers omitted due to high min-score?
  (if (not (available-meta-hyps "meta-rej-minscore")) []
      (let [minscore (/ (double (:MinScore params)) 100.0)
            {:keys [implicated may-resolve]} (find-rej-minscore-candidates
                                              problem-cases cur-ws expl)] 
        (if (not-empty implicated)
          (let [new-minscore (* 100.0 (- (apply min (map :apriori implicated)) 0.01))]
            [(new-hyp "TooHighMinScore" :meta-rej-minscore :meta-rej-minscore
                      0.25 false meta-hyp-conflicts?
                      (map :contents may-resolve)
                      "Explainers rejected due to too-high min-score"
                      (format "These explainers were rejected due to too-high min-score:\n%s\n\nLowering to: %.2f\n\nRelevant problem cases:\n%s"
                         (str/join "\n" (sort (map str implicated)))
                         new-minscore
                         (str/join "\n" (sort (map str may-resolve))))
                      {:action (partial action-lower-minscore new-minscore time-now)
                       :cycle (:cycle (cur-ep (goto-start-of-time est time-now)))
                       :implicated implicated
                       :new-minscore new-minscore
                       :min-score-delta (- minscore (apply min (map :apriori implicated)))
                       :max-score-delta (- minscore (apply max (map :apriori implicated)))
                       :avg-score-delta (- minscore (avg (map :apriori implicated)))})])
          []))))

(defn make-meta-hyps
  "Create explanations, and associated actions, for problem-cases."
  [problem-cases est time-prev time-now]
  (let [available-meta-hyps (set (str/split (:MetaHyps params) #","))
        cur-ws (:workspace (cur-ep est))
        expl (set (mapcat #(explainers cur-ws %) problem-cases))]
    (concat
     (make-meta-hyps-order-dep
      problem-cases est time-prev time-now available-meta-hyps cur-ws expl)
     (make-meta-hyps-rej-conflict
      problem-cases est time-prev time-now available-meta-hyps cur-ws expl)
     (make-meta-hyps-rej-minscore
      problem-cases est time-prev time-now available-meta-hyps cur-ws expl))))

(defn score-meta-hyps-estimate
  [problem-cases meta-hyps est time-prev time-now sensors]
  [est meta-hyps])

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
                              :apriori (cond (= "diff" (:ScoreMetaHyps params))
                                             (max 0.0
                                                  (- (avg (map :apriori problem-cases))
                                                     (avg (map :apriori problem-cases-new))
                                                     (if (nil? (:penalty hyp)) 0.0
                                                         (:penalty hyp))))
                                             ;; "doubt"
                                             :else
                                             doubt-new)
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
    (score-meta-hyps-estimate problem-cases meta-hyps est time-prev time-now sensors)
    (score-meta-hyps-simulate problem-cases meta-hyps est time-prev time-now sensors)))

(defn meta-abductive
  [problem-cases est time-prev time-now sensors]
  (let [meta-params (assoc params
                      :MinScore (:MetaMinScore params)
                      :Threshold (:MetaThreshold params)
                      :GetMoreHyps false)
        meta-hyps (make-meta-hyps problem-cases est time-prev time-now)
        [est-new meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps
                                                    est time-prev time-now sensors)
        meta-est (new-child-ep (init-est (assoc (init-workspace)
                                           :meta-oracle (:meta-oracle (:workspace (cur-ep est))))))
        meta-ws (binding [params meta-params]
                  (reduce #(add %1 %2 0)
                     (reduce #(add-observation %1 %2 0)
                        (:workspace (cur-ep meta-est)) problem-cases)
                     meta-hyps-scored))
        meta-est-reasoned (binding [params meta-params]
                            (reason (update-est meta-est (assoc (cur-ep meta-est)
                                                           :workspace meta-ws))
                                    0 1 nil :no-metareason))
        meta-ws-reasoned (:workspace (cur-ep meta-est-reasoned))
        ;; take out the "observations"
        meta-accepted (filter (fn [hyp] (not ((set (map :contents problem-cases)) (:contents hyp))))
                         (map #(lookup-hyp meta-ws-reasoned %)
                            (:all (:accepted meta-ws-reasoned))))
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
                       (meta-apply-and-evaluate est-new-meta-est est-new time-now sensors))]
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
    (let [{:keys [est-old est-new best]}
          (meta-abductive problem-cases est time-prev time-now sensors)
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

(defn meta-rule-based
  [problem-cases est time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est-prior est
         attempted #{}
         rules-applied #{}]
    (if (empty? problem-cases)
      {:est-old (goto-ep est-prior (:id (cur-ep est)))
       :est-new est-prior}
      (let [meta-hyps (make-meta-hyps problem-cases est-prior time-prev time-now)
            [est-scored meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps
                                                           est-prior time-prev time-now sensors)
            meta-est (new-child-ep (init-est (init-workspace)))
            meta-ws (reduce #(add %1 %2 0)
                       (reduce #(add-observation %1 %2 0)
                          (:workspace (cur-ep meta-est)) problem-cases)
                       meta-hyps-scored)
            do-action (fn [h] (let [meta-ws-accepted (accept meta-ws h nil [] [] 0.0 {} 0) ;; a kludge
                                   meta-est-accepted (update-est meta-est
                                                                 (assoc (cur-ep meta-est)
                                                                   :workspace meta-ws-accepted))
                                   est-with-meta-hyps (update-est est-scored
                                                                  (assoc (cur-ep est-scored)
                                                                    :meta-est meta-est-accepted))
                                   [est-action params-action] ((:action h) est-with-meta-hyps)]
                               (binding [params params-action]
                                 (:est-new (meta-apply-and-evaluate
                                            est-with-meta-hyps est-action time-now sensors)))))
            batch1-hyp (first (filter #(and (= :meta-order-dep (:type %))
                                       (= 1 (:time-delta %)))
                                 meta-hyps-scored))
            least-minscore-hyp (last (sort-by :new-minscore (filter #(= :meta-rej-minscore (:type %))
                                                               meta-hyps-scored)))
            cheapest-rej-conflict-hyp (last (sort-by :cycle (filter #(= :meta-rej-conflict (:type %))
                                                               meta-hyps-scored)))
            rule (cond (= "a" (:MetaRuleSet params))
                       (cond (and (not (rules-applied :batch1)) batch1-hyp)
                             :batch1
                             (and (not (rules-applied :lower-minscore)) least-minscore-hyp)
                             :lower-minscore
                             (and (not (rules-applied :reject-conflict)) cheapest-rej-conflict-hyp)
                             :reject-conflict)
                       (= "b" (:MetaRuleSet params))
                       (cond (and (not (rules-applied :reject-conflict)) cheapest-rej-conflict-hyp)
                             :reject-conflict
                             (and (not (rules-applied :lower-minscore)) least-minscore-hyp)
                             :lower-minscore
                             (and (not (rules-applied :batch1)) batch1-hyp)
                             :batch1)
                       (= "c" (:MetaRuleSet params))
                       (cond (and (not (rules-applied :lower-minscore)) least-minscore-hyp)
                             :lower-minscore
                             (and (not (rules-applied :reject-conflict)) cheapest-rej-conflict-hyp)
                             :reject-conflict
                             (and (not (rules-applied :batch1)) batch1-hyp)
                             :batch1))
            choice (cond (= :batch1 rule) batch1-hyp
                         (= :lower-minscore rule) least-minscore-hyp
                         (= :reject-conflict rule) cheapest-rej-conflict-hyp)]
        (comment
          (println "problem-cases:" problem-cases)
          (println "batch1-hyp:" batch1-hyp)
          (println "least-minscore-hyp:" least-minscore-hyp)
          (println "cheapest-rej-conflict-hyp:" cheapest-rej-conflict-hyp)
          (println "rule:" rule)
          (println "choice:" choice)
          (println "\n\n"))
        (if choice
          (let [est-result (do-action choice)
                problem-cases-new (find-problem-cases est-result)]
            (recur problem-cases-new est-result
                   (conj attempted (dissoc (:contents choice) :action))
                   (conj rules-applied rule)))
          ;; no choice made; we're done
          (let [meta-est-hyps (update-est meta-est (assoc (cur-ep meta-est) :workspace meta-ws))
                est-with-meta-hyps (update-est est-scored (assoc (cur-ep est-scored)
                                                            :meta-est meta-est-hyps))]
            {:est-old (goto-ep est-with-meta-hyps (:id (cur-ep est)))
             :est-new est-with-meta-hyps}))))))

(defn resolve-by-ignoring
  [problem-cases est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-old (:workspace (cur-ep est))
        ws-ignored (reject-many (:workspace new-ep) problem-cases :ignoring (:cycle new-ep))
        new-est-ignored (update-est new-est (assoc new-ep :workspace ws-ignored))]
    (reason new-est-ignored time-prev time-now sensors :no-metareason)))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :Metareasoning)"
  [est time-prev time-now sensors]
  (let [problem-cases (find-problem-cases est)
        m (:Metareasoning params)
        f (cond (= "batch" m)
                meta-batch1
                (= "lower-minscore" m)
                meta-lower-minscore
                (= "rej-conflict" m)
                meta-rej-conflict
                (= "abd" m)
                meta-abductive-recursive
                (= "rule-based" m)
                meta-rule-based
                (= "ignore" m)
                (constantly nil))
        result (f problem-cases est time-prev time-now sensors)
        problem-cases-old (if result (find-problem-cases (:est-old result))
                              problem-cases)
        problem-cases-new (when result (find-problem-cases (:est-new result)))]
    (cond (nil? result)
          (resolve-by-ignoring problem-cases-old est
                               time-prev time-now sensors)
          (empty? problem-cases-new)
          (:est-new result)
          (<= (count problem-cases-new) (count problem-cases-old))
          (resolve-by-ignoring problem-cases-new (:est-new result)
                               time-prev time-now sensors)
          :else
          (resolve-by-ignoring problem-cases-old (:est-old result)
                               time-prev time-now sensors))))
