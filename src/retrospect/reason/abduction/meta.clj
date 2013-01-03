(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est
          update-est goto-start-of-time print-est goto-ep ep-path goto-cycle]])
  (:use [retrospect.reason.abduction.workspace])
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
                    (update-hypotheses ws-sensors time-now)
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

(defn meta-batch
  [n problem-caes est _ time-now sensors]
  (let [time (if n (- time-now n) 0)
        new-est (new-branch-ep est (cur-ep (goto-start-of-time est time)))]
    (meta-apply-and-evaluate est new-est time-now sensors)))

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
  [time-now est]
  [(new-branch-ep est (cur-ep (goto-start-of-time est time-now)))
   (assoc params :MinScore (double (/ (:MinScore params) 2)))])

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

(defn meta-hyp-conflicts?
  [hyp1 hyp2]
  (and (not= hyp1 hyp2)
       (not-empty (set/intersection (set (:explains hyp1)) (set (:explains hyp2))))))

(comment
  (let [doubts-times
        (reverse (sort-by first
                          (filter first
                             (map (fn [ep]
                                  [(calc-doubt (:workspace ep)) (:time ep)])
                                (filter #((set (range 2 (dec time-now))) (:time %))
                                   (ep-path est))))))
        highest-doubt (ffirst doubts-times)
        prior-weakest (when highest-doubt
                        (dec (second (first (sort-by second
                                                     (filter #(= highest-doubt (first %))
                                                        doubts-times))))))]))

(defn make-meta-hyps-order-dep
  [problem-cases est time-prev time-now available-meta-hyps cur-ws expl]
  (if (not (available-meta-hyps "meta-order-dep")) []
      ;; order dependency among the observations; a no-expl-offered situation
      (if (not= 0 time-prev)
        (let [batchbeg-ep (cur-ep (goto-start-of-time est 0))
              batchbeg-hyp (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
                                    1.0 false meta-hyp-conflicts? []
                                    (format "Order dependency at time 0, ep %s" (str batchbeg-ep))
                                    (format "Order dependency at time 0, ep %s" (str batchbeg-ep))
                                    {:action (partial action-batch batchbeg-ep)
                                     :cycle (:cycle batchbeg-ep)
                                     :time 0})]
          (loop [hs (filter #(empty? (explainers cur-ws %)) problem-cases)
                 order-dep-hyps [batchbeg-hyp]]
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
                                     1.0 false meta-hyp-conflicts? []
                                     (format "Order dependency at time %d, ep %s" (dec t) (str ep))
                                     (format "Order dependency at time %d, ep %s" (dec t) (str ep))
                                     {:action (partial action-batch ep)
                                      :cycle (:cycle ep)
                                      :time (dec t)}))))))))
        ;; time-prev == 0, so this is a "static" case or we have not
        ;; done much reasoning yet
        [])))

(defn make-meta-hyps-rej-conflict
  [problem-cases est time-prev time-now available-meta-hyps cur-ws expl]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (if (not (available-meta-hyps "meta-rej-conflict")) []
      (let [expl-rejected-conflicts (filter (fn [h] (= :conflict (rejection-reason cur-ws h)))
                                       expl)
            acc-conflicting (set (mapcat
                                  (fn [e] (filter (fn [c] (accepted-before? cur-ws c e))
                                            (find-conflicts-all cur-ws e)))
                                  expl-rejected-conflicts))
            inner-hyps (set (map :id (mapcat :hyps acc-conflicting)))
            acc-conflicting-no-comps (filter #(not (inner-hyps (:id %)))
                                        acc-conflicting)
            ;; which ep-states rejected these expl (essentials are
            ;; allowed; we may still want to reject an "essential"
            ;; explainer
            ep-rejs (filter (fn [ep] (some (set (map :id acc-conflicting-no-comps))
                                  (:acc (:accrej (:workspace ep)))))
                       (ep-path est))
            rejs-deltas (map (fn [ep] [(get-in ep [:workspace :accrej :delta])
                                    (:cycle ep)
                                    (get-in ep [:workspace :accrej :best])])
                           ep-rejs)
            bad-bests (reverse
                       (sort-by first (set (filter (fn [[delta _ best]] (and delta best))
                                              rejs-deltas))))]
        (for [[delta cycle hyp] bad-bests]
          (new-hyp "RejConflict" :meta-rej-conflict :meta-rej-conflict
                   0.5 false meta-hyp-conflicts? []
                   (format "%s rejected some explainers" hyp)
                   (format "%s rejected these explainers (cycle %d, delta %.2f):\n%s"
                      (str hyp) cycle delta (str/join "\n" (map str expl-rejected-conflicts)))
                   {:action (partial action-preemptively-reject hyp cycle)
                    :cycle cycle
                    :implicated hyp})))))

(defn make-meta-hyps-rej-minscore
  [problem-cases est time-prev time-now available-meta-hyps cur-ws expl]
  ;; were some explainers omitted due to high min-score?
  (if (not (available-meta-hyps "meta-rej-minscore")) []
      (let [expl-rejected-minscore (filter (fn [h]
                                        (and (= :minscore (rejection-reason cur-ws h))
                                             ;; require that hyp had a score at least
                                             ;; half the minscore
                                             (>= (:apriori h) (/ (double (:MinScore params)) 200.0))))
                                      expl)]
        (if (not-empty expl-rejected-minscore)
          [(new-hyp "TooHighMinScore" :meta-rej-minscore :meta-rej-minscore
                    0.25 false meta-hyp-conflicts? []
                    "Explainers rejected due to too-high min-score"
                    (format "These explainers were rejected due to too-high min-score: %s"
                       (str/join ", " (sort (map str expl-rejected-minscore))))
                    {:action (partial action-lower-minscore time-now)
                     :cycle (:cycle (cur-ep (goto-start-of-time est time-now)))
                     :implicated expl-rejected-minscore})]
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
  meta-hyps)

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
              doubt-new (doubt-aggregate (:est-new result))
              problem-cases-new (find-problem-cases (:est-new result))
              resolved-cases (filter (fn [pc] (not ((set (map :contents problem-cases-new))
                                              (:contents pc))))
                                problem-cases)]
          (recur (:est-old result) (rest hyps)
                 (conj new-hyps
                       (assoc hyp :explains (map :contents resolved-cases)
                              :resolves resolved-cases
                              :final-ep-id (:id (cur-ep (:est-new result)))
                              :apriori (- 1.0 doubt-new)
                              :desc (format "%s\n\nEp-state start: %s"
                                       (:desc hyp) (str (cur-ep est-new))))))))))

(defn score-meta-hyps
  [problem-cases meta-hyps est time-prev time-now sensors]
  (if (:EstimateMetaScores params)
    (score-meta-hyps-estimate problem-cases meta-hyps est time-prev time-now sensors)
    (score-meta-hyps-simulate problem-cases meta-hyps est time-prev time-now sensors)))

(defn meta-abductive
  [problem-cases est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps problem-cases est time-prev time-now)
        [est-new meta-hyps-scored] (score-meta-hyps problem-cases meta-hyps
                                                    est time-prev time-now sensors)
        meta-est (new-child-ep (init-est (assoc (init-workspace)
                                           :meta-oracle (:meta-oracle (:workspace (cur-ep est))))))
        meta-ws (reduce add (reduce #(add-observation %1 %2 0)
                          (:workspace (cur-ep meta-est)) problem-cases)
                   meta-hyps-scored)
        meta-est-reasoned (binding [params (assoc params
                                             :MinScore (:MetaMinScore params)
                                             :Threshold (:MetaThreshold params)
                                             :GetMoreHyps false)]
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
                                            :else 0)
                                      (compare (:apriori h2) (:apriori h1))))
        best (first (sort meta-hyp-compare meta-accepted))]
    {:est-old (goto-ep est-new-meta-est (:id (cur-ep est)))
     :est-new (if (nil? best) est-new-meta-est
                  (goto-ep est-new-meta-est (:final-ep-id best)))
     :best best}))

(defn meta-abductive-recursive
  [problem-cases est time-prev time-now sensors]
  (loop [problem-cases problem-cases
         est est
         attempted #{}
         implicated #{}]
    (let [{:keys [est-old est-new best]}
          (meta-abductive problem-cases est time-prev time-now sensors)]
      (if (and best
               (not (attempted (:contents best)))
               (or (nil? (:implicated best))
                   (not (implicated (:contents (:implicated best))))))
        (recur (find-problem-cases est-new)
               est-new
               (conj attempted (:contents best))
               (if (:implicated best)
                 (conj implicated (:contents (:implicated best)))
                 implicated))
        {:est-old est-old :est-new est-new}))))

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
        f (cond (= "batchbeg" m)
                (partial meta-batch nil)
                (= "batch1" m)
                (partial meta-batch 1)
                (= "batch2" m)
                (partial meta-batch 2)
                (= "batch3" m)
                (partial meta-batch 3)
                (= "batch4" m)
                (partial meta-batch 4)
                (= "batch5" m)
                (partial meta-batch 5)
                (= "batch-mid" m)
                (partial meta-batch (int (/ (count (set (map :time (ep-path est)))) 2)))
                (= "batch-weakest" m)
                meta-batch-weakest
                (= "abd" m)
                meta-abductive
                (= "abd-recursive" m)
                meta-abductive-recursive
                (= "ignore" m)
                (constantly nil))
        result (f problem-cases est time-prev time-now sensors)
        problem-cases-old (if result (find-problem-cases (:est-old result)) problem-cases)
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
