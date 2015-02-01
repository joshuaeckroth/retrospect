(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:require [paragon.core :as paragon])
  (:use [clojure.math.combinatorics :only [combinations]])
  (:use [retrospect.epistemicstates])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.evaluate :only [avg]])
  (:use [retrospect.reason.abduction.evaluate :only
         [doubt-aggregate some-noexp-reason?]])
  (:use [retrospect.logging])
  (:use [retrospect.state])
  (:require [geppetto.random :as random])
  (:use [taoensso.timbre.profiling :only [defnp profile]]))

;; basic reasoning process
;;{{{

(declare metareason)

(defn find-anomalies
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (if (:AnomaliesUnexp params)
      (unexplained workspace)
      (no-explainers workspace))))

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
  [workspace cycle time-now meta?]
  (binding [reason-log (ref (:log workspace))
            doing-meta? meta?]
    (log "Explaining at cycle" cycle)
    (let [ws (explain workspace cycle time-now)]
      (assoc ws :log @reason-log))))

(defn jg-rand
  [_ _ bad-strokes bad-nodes]
  (random/my-rand-nth (sort-by paragon/jgstr (concat bad-strokes bad-nodes))))

(defn jg-rand-pref-node
  [_ _ bad-strokes bad-nodes]
  (if (not-empty bad-nodes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-nodes))
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))))

(defn jg-rand-pref-stroke
  [_ _ bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (random/my-rand-nth (sort-by paragon/jgstr bad-nodes))))

(defn jg-score-node-black
  [ws _ bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; choose highest apriori
        (let [scored-nodes (map (fn [hypid] [hypid (:apriori (lookup-hyp ws hypid))]) bad-nodes)
              best-node (first (last (sort-by second (filter second scored-nodes))))]
          #_(println "Black / Scored nodes:" (sort-by second (filter second scored-nodes)))
          #_(println "Black / Best node:" best-node)
          (or best-node (random/my-rand-nth (sort-by paragon/jgstr bad-nodes))))))))

(defn jg-score-incompat-node-black
  [ws jg bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; choose highest apriori, avoiding nodes that would color 2+ of an incompat group black
        (let [scored-nodes (map (fn [hypid] [hypid (:apriori (lookup-hyp ws hypid))]) bad-nodes)
              scored-non-incompat (filter (fn [[n score]]
                                            (let [bots (filter paragon/bottom? (paragon/jgout jg n))]
                                              (or (empty? bots)
                                                  (not-any? (fn [s] (some (fn [n2] (paragon/black? jg n2))
                                                                          (paragon/jgin jg s)))
                                                            bots))))
                                          scored-nodes)
              best-node (first (last (sort-by second (filter second scored-non-incompat))))]
          #_(println "Black / Scored nodes:" (sort-by second (filter second scored-nodes)))
          #_(println "Black / Scored non-incompat nodes:" (sort-by second (filter second scored-non-incompat)))
          #_(println "Black / Best node:" best-node)
          best-node)))))

(defn jg-score-node-white
  [ws _ bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; choose highest apriori
        (let [scored-nodes (map (fn [hypid] [hypid (:apriori (lookup-hyp ws hypid))]) bad-nodes)
              best-node (first (first (sort-by second (filter second scored-nodes))))]
          #_(println "White / Scored nodes:" (sort-by second (filter second scored-nodes)))
          #_(println "White / Best node:" best-node)
          (or best-node (random/my-rand-nth (sort-by paragon/jgstr bad-nodes))))))))

(defn jg-essential-score-node-black
  [ws jg bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; choose an essential explainer if it exists
        ;; an essential hypothesis is the sole explainer for something it explains
        (let [essentials (filter (fn [n] (and (paragon/hypothesis? jg n)
                                              (some (fn [exp] (= [n] (paragon/explainers jg exp)))
                                                    (paragon/explains jg n))))
                                 bad-nodes)]
          #_(println "Black / Essentials:" essentials)
          (if (not-empty essentials)
            ;; choose highest apriori essential
            (last (sort-by (fn [hypid] (:apriori (lookup-hyp ws hypid))) essentials))
            (jg-score-node-black ws jg bad-strokes bad-nodes)))))))

(defn jg-essential-score-node-white
  [ws jg bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; AVOID an essential explainer if it exists; prefer a non-essential hypothesis
        ;; an essential hypothesis is the sole explainer for something it explains
        (let [non-essentials (filter (fn [n] (and (paragon/hypothesis? jg n)
                                                  (every? (fn [exp] (< 1 (count (paragon/explainers jg exp))))
                                                          (paragon/explains jg n))))
                                     bad-nodes)]
          #_(println "White / Non-essentials:" non-essentials)
          (if (not-empty non-essentials)
            ;; choose lowest-apriori NON-essential
            (let [best-node (first (sort-by (fn [hypid] (:apriori (lookup-hyp ws hypid))) non-essentials))]
              #_(println "Choosing" best-node)
              best-node)
            (jg-score-node-white ws jg bad-strokes bad-nodes)))))))

(defn jg-obs-essential-score-node-black
  [ws jg bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; choose an observation if it exists
        (let [obs (filter (fn [n] (= :observation (:type (lookup-hyp ws n)))) bad-nodes)]
          (if (not-empty obs)
            ;; choose highest apriori obs
            (last (sort-by (fn [hypid] (:apriori (lookup-hyp ws hypid))) obs))
            ;; choose an essential explainer if it exists
            ;; an essential hypothesis is the sole explainer for something it explains
            (let [essentials (filter (fn [n] (and (paragon/hypothesis? jg n)
                                                  (some (fn [exp] (= [n] (paragon/explainers jg exp)))
                                                        (paragon/explains jg n))))
                                     bad-nodes)]
              (if (not-empty essentials)
                ;; choose highest apriori essential
                (last (sort-by (fn [hypid] (:apriori (lookup-hyp ws hypid))) essentials))
                (jg-score-node-black ws jg bad-strokes bad-nodes)))))))))

(defn jg-obs-essential-score-node-white
  [ws jg bad-strokes bad-nodes]
  (if (not-empty bad-strokes)
    (random/my-rand-nth (sort-by paragon/jgstr bad-strokes))
    (let [non-hyps (filter (fn [n] (not (integer? n))) bad-nodes)]
      (if (not-empty non-hyps)
        (random/my-rand-nth (sort-by paragon/jgstr non-hyps))
        ;; AVOID an observation or essential explainer if it exists; prefer a non-essential hypothesis
        ;; an essential hypothesis is the sole explainer for something it explains
        (let [non-essentials (filter (fn [n] (and (paragon/hypothesis? jg n)
                                                  (every? (fn [exp] (< 1 (count (paragon/explainers jg exp))))
                                                          (paragon/explains jg n))))
                                     bad-nodes)]
          (if (not-empty non-essentials)
            ;; choose lowest-apriori NON-observation/NON-essential
            (first (sort-by (fn [hypid] (:apriori (lookup-hyp ws hypid))) non-essentials))
            (let [non-obs (filter (fn [n] (not= :observation (:type (lookup-hyp ws n)))) bad-nodes)]
              (if (not-empty non-obs)
                ;; prefer lowest-scoring non-observation
                (first (sort-by (fn [hypid] (:apriori (lookup-hyp ws hypid))) non-obs))
                (jg-score-node-white ws jg bad-strokes bad-nodes)))))))))

(defn jg-lookup-black-strategy
  [strat]
  (case strat
    "rand" jg-rand
    "rand-pref-node" jg-rand-pref-node
    "rand-pref-stroke" jg-rand-pref-stroke
    "score" jg-score-node-black
    "ess-score" jg-essential-score-node-black
    "obs-ess-score" jg-obs-essential-score-node-black
    "score-incompat" jg-score-incompat-node-black
    nil))

(defn jg-lookup-white-strategy
  [strat]
  (case strat
    "rand" jg-rand
    "rand-pref-node" jg-rand-pref-node
    "rand-pref-stroke" jg-rand-pref-stroke
    "score" jg-score-node-white
    "ess-score" jg-essential-score-node-white
    "obs-ess-score" jg-obs-essential-score-node-white
    "score-incompat" jg-score-node-black
    nil))

(defn explain-and-advance
  [est time-prev time-now sensors meta?]
  (let [ws (:workspace (cur-ep est))
        cycle (:cycle (cur-ep est))
        ws-hyps (workspace-update-hypotheses ws time-prev time-now sensors cycle)
        ws-explained (workspace-explain ws-hyps cycle time-now meta?)
        est-result (est-workspace-child est ws-explained)]
    (if (or (and (:GetMoreHyps params)
                 (not= (count (:hyp-ids ws-explained))
                       (count (:hyp-ids ws))))
            (:best (:accrej ws-explained)))
      ;; don't recur with sensors so that sensor hyps are not re-added
      (recur est-result time-prev time-now nil meta?)
      est-result)))

(defn execute-paragon
  [ws]
  (let [new-jg (paragon/expand (:jg ws) (map :id (:observation (hypotheses ws)))
                               :white-strategy (partial (or (jg-lookup-white-strategy (:ParagonStrategy params))
                                                            (jg-lookup-white-strategy (:ParagonWhiteStrategy params))
                                                            (fn [_ jg bad-strokes bad-nodes]
                                                              (paragon/spread-white-default-strategy
                                                                jg bad-strokes bad-nodes)))
                                                        ws)
                               :black-strategy (partial (or (jg-lookup-black-strategy (:ParagonStrategy params))
                                                            (jg-lookup-black-strategy (:ParagonBlackStrategy params))
                                                            (fn [_ jg bad-strokes bad-nodes]
                                                              (paragon/spread-white-default-strategy
                                                                jg bad-strokes bad-nodes)))
                                                        ws))]
    #_(paragon/visualize new-jg)
    (assoc ws :jg new-jg)))

(defn reason
  [est time-prev time-now sensors & opts]
  (loop [est est]
    (let [meta? (some #{:no-metareason} opts)
          est-new (explain-and-advance est time-prev time-now sensors meta?)
          activate-meta? (and (not meta?)
                     (metareasoning-activated? est-new))
          est-meta (cond (not activate-meta?) est-new
                         (= 0 (mod time-now (:MetaEveryNSteps params)))
                         (metareason est-new time-prev time-now sensors)
                         :else est-new)]
      ;; if something was accepted last, repeat
      (if (:best (:accrej (:workspace (cur-ep est-meta))))
        (recur est-meta)
        (update-est est-meta (update-in (cur-ep est-meta) [:workspace] execute-paragon))))))

(defn meta-apply
  [est est-new time-prev time-now sensors]
  (let [reason-est (reason est-new time-prev time-now sensors :no-metareason)]
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

(defnp resolve-conf-exp
  [rej-hyp est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws (-> (:workspace ep)
               (undecide rej-hyp (:cycle ep))
               (prevent-undecide rej-hyp)
               (reject rej-hyp :preemptive (:cycle ep)))
        ep-acc (assoc ep :workspace ws)]
    [(update-est new-est ep-acc) params]))

(defnp conf-exp-candidates
  [anomalies est time-prev time-now sensors]
  (let [cur-ws (:workspace (cur-ep est))
        rel-anomalies (set (filter #(and (no-explainers? cur-ws %)
                                         (some-noexp-reason? cur-ws % :conflict)) anomalies))
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
        ep-rejs-deltas (map (fn [ep]
                              (let [hyp (get-in ep [:workspace :accrej :best])]
                                {:delta (get-in ep [:workspace :accrej :delta])
                                 :cycle (:cycle ep)
                                 :time (:time ep)
                                 :rejected-expl (filter (fn [h] (conflicts? hyp h)) expl-rc)
                                 :hyp hyp}))
                            ep-rejs)
        earliest-rejs-deltas (for [hyp (sort-by :id (map :hyp ep-rejs-deltas))]
                               (first (sort-by :cycle (filter #(= hyp (:hyp %)) ep-rejs-deltas))))]
    (doall (filter (fn [meta-hyp]
                     (and (not-empty (:may-resolve meta-hyp))
                          (or (not (:RemoveEssentialConfExp params))
                              (not (:essential? meta-hyp)))))
                   (for [{:keys [delta cycle time hyp rejected-expl]} earliest-rejs-deltas]
                     (if (:SimulateSomeMetaHyps params)
                       ;; do a simulation to figure out which anomalies are resolved
                       (let [[est-resolved _] (resolve-conf-exp hyp est time-prev time-now nil)
                             est-reasoned (:est-new (meta-apply est est-resolved time-prev time-now nil))
                             anomalies-resolved (set/difference rel-anomalies (set (find-anomalies est-reasoned)))]
                         {:rej-hyp hyp :cycle cycle :time time :delta delta
                          :rejected-expl rejected-expl :may-resolve anomalies-resolved
                          :essential? (empty? (accepted-rivals cur-ws hyp))})
                       {:rej-hyp hyp :cycle cycle :time time :delta delta
                        :rejected-expl rejected-expl
                        :may-resolve (set/intersection (set (mapcat #(explains cur-ws %) rejected-expl))
                                                       rel-anomalies)
                        :essential? (empty? (accepted-rivals cur-ws hyp))}))))))

(defnp make-meta-hyps-conflicting-explainers
  [anomalies est time-prev time-now sensors]
  ;; correct explainer(s) were rejected due to conflicts; need to
  ;; consider the various possibilities of rejected explainers and
  ;; no-explainers combinations
  (for [{:keys [rej-hyp cycle time delta rejected-expl may-resolve essential?]}
        (conf-exp-candidates anomalies est time-prev time-now sensors)]
    (let [apriori (cond (= "opt1" (:ScoreMetaConfExp params))
                        (* (avg (map :apriori may-resolve)) (- 1.0 (:apriori rej-hyp)))
                        (= "opt2" (:ScoreMetaConfExp params))
                        (avg (map :apriori may-resolve))
                        (= "opt3" (:ScoreMetaConfExp params))
                        (avg (conj (map :apriori may-resolve) (- 1.0 delta)))
                        (= "opt4" (:ScoreMetaConfExp params))
                        (avg (conj (map :apriori may-resolve) (- 1.0 (:apriori rej-hyp))))
                        (= "opt5" (:ScoreMetaConfExp params))
                        (- 1.0 (:apriori rej-hyp))
                        (= "opt6" (:ScoreMetaConfExp params))
                        (/ (avg (map :apriori may-resolve)) (inc (- (:cycle (cur-ep est)) cycle)))
                        (= "opt7" (:ScoreMetaConfExp params))
                        (/ (avg (map :apriori may-resolve)) (inc (- (:time (cur-ep est)) time)))
                        (= "opt8" (:ScoreMetaConfExp params))
                        (/ (- 1.0 (:apriori rej-hyp)) (inc (- (:cycle (cur-ep est)) cycle)))
                        (= "opt9" (:ScoreMetaConfExp params))
                        (/ (- 1.0 (:apriori rej-hyp)) (inc (- (:time (cur-ep est)) time)))
                        :else
                        (* (avg (map :apriori may-resolve)) (- 1.0 (:apriori rej-hyp))))]
      (new-hyp "ConfExp" :meta-conf-exp :meta-conf-exp apriori
               false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
               (map :contents may-resolve)
               (format "%s rejected some hyps" (:name rej-hyp))
               (format "%s rejected %s at cycle %d with delta %.2f"
                       rej-hyp (str/join ", " (sort-by :id rejected-expl)) cycle delta)
               {:action (partial resolve-conf-exp rej-hyp)
                :resolves may-resolve
                :rej-hyp rej-hyp
                :implicated rej-hyp
                :rejected-expl rejected-expl
                :cycle cycle
                :cycle-diff (- (:cycle (cur-ep est)) cycle)
                :time-diff (- (:time (cur-ep est)) time)
                :delta delta
                :essential? essential?
                :attempted-key [:conf-exp rej-hyp]}))))
;;}}}

;; implausible explainers
;;{{{

(defnp resolve-impl-exp
  [hyp est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        ep (cur-ep new-est)
        ws (-> (:workspace ep)
               (undecide hyp (:cycle ep))
               (prevent-undecide hyp)
               (prevent-rejection hyp :minscore))
        ep-prev-minscore (assoc ep :workspace ws)]
    [(update-est new-est ep-prev-minscore) params]))

(defnp impl-exp-candidates
  [anomalies est time-prev time-now sensors]
  (let [cur-ws (:workspace (cur-ep est))
        rel-anomalies (set (filter #(and (no-explainers? cur-ws %)
                                         (some-noexp-reason? cur-ws % :minscore))
                                   anomalies))
        ;; explainers of anomalies
        expl (set (mapcat #(explainers cur-ws %) rel-anomalies))
        ;; explainers that were rejected due to minscore
        expl-rejected-minscore (sort-by :id (filter (fn [h] (= :minscore (rejection-reason cur-ws h))) expl))]
    (doall (filter #(not-empty (:may-resolve %))
                   (for [hyp expl-rejected-minscore]
                     (if (:SimulateSomeMetaHyps params)
                       ;; do a simulation to figure out which anomalies are resolved
                       (let [[est-resolved _] (resolve-impl-exp hyp est time-prev time-now nil)
                             est-reasoned (:est-new (meta-apply est est-resolved time-prev time-now nil))
                             anomalies-resolved (set/difference rel-anomalies (set (find-anomalies est-reasoned)))]
                         {:acc-hyp hyp
                          :may-resolve anomalies-resolved
                          :score-delta (- (/ (:MinScore params) 100.0) (:apriori hyp))})
                       {:acc-hyp hyp
                        :may-resolve (set/intersection rel-anomalies (set (explains cur-ws hyp)))
                        :score-delta (- (/ (:MinScore params) 100.0) (:apriori hyp))}))))))

(defnp make-meta-hyps-implausible-explainers
  [anomalies est time-prev time-now sensors]
  ;; were some explainers omitted due to high min-score?
  (let [candidates (impl-exp-candidates anomalies est time-prev time-now sensors)
        meta-hyps (for [{:keys [acc-hyp may-resolve score-delta]} candidates]
                    (let [conflicts-with-accepted? (some (partial conflicts? acc-hyp)
                                                         (:all (accepted (:workspace (cur-ep est)))))
                          apriori (cond (= "opt1" (:ScoreMetaImplExp params))
                                        (avg (conj (map :apriori may-resolve) (:apriori acc-hyp)))
                                        (= "opt2" (:ScoreMetaImplExp params))
                                        (avg (map :apriori may-resolve))
                                        (= "opt3" (:ScoreMetaImplExp params))
                                        (:apriori acc-hyp)
                                        :else
                                        (avg (conj (map :apriori may-resolve) (:apriori acc-hyp))))]
                      (new-hyp "ImplExp" :meta-impl-exp :meta-impl-exp apriori
                               false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
                               (map :contents may-resolve)
                               "Explainer rejected due to min-score"
                               (format "%s was rejected due to min-score\n\nConflicts with accepted? %s\nScore delta: %.2f"
                                       acc-hyp (str conflicts-with-accepted?) score-delta)
                               {:action (partial resolve-impl-exp acc-hyp)
                                :resolves may-resolve
                                :acc-hyp acc-hyp
                                :score-delta score-delta
                                :implicated acc-hyp
                                :conflicts-with-accepted? conflicts-with-accepted?
                                :attempted-key [:impl-exp acc-hyp]})))
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

(defnp resolve-order-dep
  [ep est time-prev time-now sensors]
  (let [new-est (new-branch-ep est ep)
        ep (cur-ep new-est)
        ws (:workspace ep)
        ws-sensors (add-sensor-hyps ws (:time ep) time-now sensors (:cycle ep))
        ws-hyps (update-hypotheses ws-sensors (:cycle ep) time-now)
        ep-batch (assoc ep :workspace ws-hyps)]
    [(update-est new-est ep-batch) params]))

(defnp order-dep-candidates
  [anomalies est]
  ;; don't batch "over" a previous batch
  (if (not= (dec (:time (cur-ep est))) (time-prior est)) []
      (let [cur-ws (:workspace (cur-ep est))
            acc (accepted cur-ws)
            rel-anomalies (filter #(and (no-explainers? cur-ws %)
                                        (some-noexp-reason? cur-ws % :no-expl-offered)) anomalies)
            accept-cycles (into {} (for [hyp rel-anomalies] [hyp (accepted-cycle cur-ws hyp)]))
            time-last (:time (cur-ep est))
            eps (filter identity
                        (map (fn [t] (let [ep (cur-ep (goto-start-of-time est t))]
                                       ;; make sure this ep is not a batch itself (e.g., want to
                                       ;; go back to start of time 4, but get an ep that starts
                                       ;; at 2 and goes to 8...)
                                       (if (= t (:time ep)) ep nil)))
                             (range (max 0 (- time-last (:MaxBatch params))) time-last)))
            candidates (for [ep eps]
                         (let [ws (:workspace ep)
                               may-resolve (filter (fn [hyp] (>= (get accept-cycles hyp) (:cycle ep))) rel-anomalies)]
                           {:may-resolve may-resolve :ep ep}))]
        (if (empty? candidates) []
            [[rel-anomalies (first (sort-by :cycle (map :ep candidates)))]]))))

(defnp make-meta-hyps-order-dep
  [anomalies est time-prev time-now sensors]
  (for [[may-resolve ep] (order-dep-candidates anomalies est)]
    (let [apriori (avg (map :apriori may-resolve))]
      (new-hyp "OrderDep" :meta-order-dep :meta-order-dep
               apriori false [:meta] (partial meta-hyp-conflicts? (:workspace (cur-ep est)))
               (map :contents may-resolve)
               (format "Order dependency at %s" (str ep))
               (format "Order dependency at %s" (str ep))
               {:action (partial resolve-order-dep ep)
                :resolves may-resolve
                :ep ep
                :attempted-key [:order-dep (:time ep)]}))))

;;}}}

;; make and score metahyps
;;{{{

(defnp make-meta-hyps
  "Create explanations, and associated actions, for anomalies."
  [anomalies est time-prev time-now sensors]
  (let [available-meta-hyps (set (str/split (:MetaHyps params) #","))
        meta-fns (filter identity
                         [(when (available-meta-hyps "meta-impl-exp")
                            make-meta-hyps-implausible-explainers)
                          (when (available-meta-hyps "meta-order-dep")
                            make-meta-hyps-order-dep)
                          (when (available-meta-hyps "meta-conf-exp")
                            make-meta-hyps-conflicting-explainers)])]
    (doall (apply concat (for [meta-fn meta-fns] (meta-fn anomalies est time-prev time-now sensors))))))

(defnp score-meta-hyps-simulate-apriori
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

(defnp score-meta-hyps-simulate
  [anomalies meta-hyps est time-prev time-now sensors]
  (loop [est-attempted est
         hyps meta-hyps
         new-hyps []]
    (if (empty? hyps) [(goto-ep est-attempted (:id (cur-ep est))) new-hyps]
        (let [hyp (first hyps)
              [est-new params-new] ((:action hyp) est-attempted time-prev time-now sensors)
              result (binding [params params-new]
                       (meta-apply est-attempted est-new time-prev time-now nil))
              doubt (doubt-aggregate est)
              doubt-new (doubt-aggregate (:est-new result))
              anomalies-new (find-anomalies (:est-new result))
              resolved-cases (filter (fn [pc] (not ((set (map :contents anomalies-new))
                                                    (:contents pc))))
                                     anomalies)
              ws-new (:workspace (cur-ep (:est-new result)))]
          (recur (:est-old result) (rest hyps)
                 (conj new-hyps
                       (assoc hyp :explains (map :contents resolved-cases)
                              :explained-obs (filter #(and (accepted? ws-new %)
                                                           (not (unexplained? ws-new %)))
                                                     (:observation (hypotheses ws-new)))
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

(defnp score-meta-hyps
  [anomalies meta-hyps est time-prev time-now sensors]
  (if (= "abd-estimate" (:Metareasoning params)) [est meta-hyps]
      (score-meta-hyps-simulate anomalies meta-hyps est time-prev time-now sensors)))

;;}}}

;; abductive metareasoning process
;;{{{

(defnp meta-abductive
  [anomalies est time-prev time-now sensors]
  (let [meta-hyps (make-meta-hyps anomalies est time-prev time-now sensors)
        ;; create different sensors for simulating only if we're in
        ;; meta-oracle mode; this is to ensure that new reports
        ;; obtained by meta-insuf-ev are forgotten again except, of
        ;; course, if the meta-insuf-ev hyp is accepted and re-applied
        sensors-simulate (if (= "oracle" (:Metareasoning params))
                           (map (fn [s] (assoc s :sensed (atom @(:sensed s)))) sensors)
                           sensors)
        [est-new meta-hyps-scored] (score-meta-hyps anomalies meta-hyps est time-prev time-now sensors-simulate)
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

(defnp meta-abductive-recursive
  [anomalies est time-prev time-now sensors]
  (loop [anomalies anomalies
         est est
         attempted #{}
         implicated #{}]
    (let [est-abd (meta-abductive anomalies est time-prev time-now sensors)
          meta-workspace (:workspace (cur-ep (:meta-est (cur-ep est-abd))))
          meta-accepted (filter (fn [h] (and (not (attempted (:attempted-key h)))
                                             (not (implicated (:contents (:implicated h))))))
                                (apply concat (vals (select-keys (accepted meta-workspace)
                                                                 (:meta-hyp-types @reasoner)))))
          est-applied (if (empty? meta-accepted) est-abd
                          (reduce (fn [est hyp]
                                    (let [[est-new params-new] ((:action hyp) est time-prev time-now sensors)]
                                      (:est-new (binding [params params-new]
                                                  (meta-apply est est-new time-prev time-now nil)))))
                                  est-abd meta-accepted))
          anomalies-new (when (not-empty meta-accepted) (find-anomalies est-applied))]
      (if (and (not-empty meta-accepted) (not-empty anomalies-new)
               (or (some (fn [h] (= :meta-order-dep (:type h))) meta-accepted)
                   (< (count anomalies-new) (count anomalies))))
        (recur anomalies-new
               est-applied
               (set/union attempted (set (map (fn [h] (:attempted-key h)) meta-accepted)))
               (set/union implicated (set (map (fn [h] (:contents (:implicated h))) meta-accepted))))
        {:est-old (goto-ep est-applied (:id (cur-ep est))) :est-new est-applied}))))

;;}}}

;; fallback: false evidence
;;{{{

(defnp resolve-false-ev
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

(defnp assume-false-evidence
  [anomalies est time-prev time-now sensors]
  (let [[new-est _] (resolve-false-ev anomalies est)]
    (reason new-est time-prev time-now sensors :no-metareason)))

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
        [anomalies-final est-final] (if (nil? result)
                                      [anomalies-old est]
                                      [anomalies-new (:est-new result)])]
    (if (:MetaRemainderIgnore params)
      (assume-false-evidence anomalies-final est-final time-prev time-now sensors)
      est-final)))


;; folded-file: t


