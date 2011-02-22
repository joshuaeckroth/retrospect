(ns samre.meta.hypotheses
  (:use [samre.workspaces :only
         [new-hyp add-hyp measure-decision-confidence force-acceptance
          clear-decision reset-confidences-to-apriori update-hyps
          lookup-hyps update-candidates-unexplained find-explainers
          reject-impossible]])
  (:use [samre.epistemicstates :only
         [flatten-ep-state-tree current-ep-state explain goto-ep-state
          new-branch-ep-state left-ep-state previous-ep-state update-ep-state-tree
          new-child-ep-state]])
  (:use [samre.meta.meta :only [have-enough-meta-hyps]])
  (:use [samre.confidences]))

(defn find-rejected-explainers
  [workspace]
  (let [unexplained (lookup-hyps workspace (:unexplained workspace))
        explainers (map #(find-explainers % (vals (:hyps workspace))) unexplained)]
    (filter #(= IMPOSSIBLE (:confidence %)) explainers)))

(defn find-no-explainers
  [workspace]
  (let [unexplained (lookup-hyps workspace (:unexplained workspace))]
    (filter #(empty? (find-explainers % (vals (:hyps workspace)))) unexplained)))

(defn mark-least-conf-impossible
  "Given an ep-state (to go back to), mark the least confident
   accepted hypothesis as IMPOSSIBLE after clearing the decision and
   resetting confidences to their apriori values. The idea is to
   ensure that the least confident (and therefore, hopefully the most
   likely incorrect) hypothesis is not accepted again, and something
   else is accepted instead."
  [ep-state]
  (if (empty? (:accepted (:decision (:workspace ep-state)))) ep-state
    (let [ws (:workspace ep-state)
          least-conf (first (sort-by :confidence (lookup-hyps ws (:accepted (:decision ws)))))]
      (assoc ep-state :workspace
             (-> ws
                 (reset-confidences-to-apriori)
                 (clear-decision)
                 (reject-impossible
                  [least-conf] "Rejecting in meta-abduction because least confident.")
                 (update-candidates-unexplained))))))

(defn branch-and-mark-impossible
  "A composite action that branches at the specified ep-state,
   then marks the least confident accepted hyp as IMPOSSIBLE and
   updates the OneRunState with all these changes."
  [ep-state-tree ep-state]
  (let [est (new-branch-ep-state ep-state-tree ep-state)
        ep (mark-least-conf-impossible (current-ep-state est))]
    (update-ep-state-tree est ep)))

(defn score-by-explaining
  [problem ep-state-tree sensors params lazy]
  (let [time-prev (if-let [time-prev (:time (previous-ep-state ep-state-tree))]
                    (inc time-prev) (:time (current-ep-state ep-state-tree)))
        time-now (apply max (map :sensed-up-to sensors))
        ep-state (explain (current-ep-state ep-state-tree) params)
        score (measure-decision-confidence (:workspace ep-state))]
    {:score score :ep-state-tree ep-state-tree}))

(defn score-by-replaying
  [problem ep-state-tree sensors params lazy]
  (let [time-now (apply max (map :sensed-up-to sensors))]
    (loop [est ep-state-tree]
      (if (< (:time (current-ep-state est)) time-now)
        (let [ep-state (current-ep-state est)
              est-child (new-child-ep-state est ep-state (:time ep-state) problem)
              ep-child (current-ep-state est-child)
              ep-hyps ((:hypothesize-fn problem) ep-child sensors (:time ep-child) params)
              ep-expl (explain ep-hyps params)]
          (recur (update-ep-state-tree est-child ep-expl)))
        {:ep-state-tree est
         :score (measure-decision-confidence (:workspace (current-ep-state est)))}))))

(defn generate-ep-state-hyp
  [ep-state]
  (new-hyp "EP" :meta NEUTRAL [] (constantly []) (constantly [])
           (constantly "ep-state") {:ep-state ep-state}))

(defn impossible-fn
  [ep-state-hyp hyp hyps]
  (filter (fn [h] (and (not= (:id hyp) (:id h))
                       (some #(= % (:id ep-state-hyp)) (:explains h)))) hyps))

(defn add-branch-hyp
  [workspace ep-state-hyp branchable problem ep-state-tree sensors params lazy]
  (let [est (branch-and-mark-impossible ep-state-tree branchable)
        ep-state (current-ep-state est)
        ep-expl (explain ep-state params)
        est-new (update-ep-state-tree est ep-expl)
        hyp (let [{score :score est-replayed :ep-state-tree}
                  (score-by-replaying problem est-new sensors params lazy)]
              (new-hyp "MH" :meta score [(:id ep-state-hyp)]
                       (constantly []) (partial impossible-fn ep-state-hyp)
                       (fn [hyp] (:id hyp))
                       {:ep-state-tree est-replayed}))]
    (add-hyp workspace hyp)))

(defn add-more-explainers-hyp
  [workspace ep-state-hyp problem ep-state-tree sensors params]
  (let [est (new-branch-ep-state ep-state-tree (:ep-state (:data ep-state-hyp)))
        ep ((:get-more-hyps-fn problem) (current-ep-state est) sensors params true)
        ep2 (assoc ep :workspace
                   (-> (:workspace ep)
                       (reset-confidences-to-apriori)
                       (clear-decision)
                       (update-candidates-unexplained)))
        est2 (update-ep-state-tree est ep2)
        hyp (let [{score :score} (score-by-explaining problem est2 sensors params true)]
              (new-hyp "MH+" :meta score [(:id ep-state-hyp)] (constantly [])
                       (partial impossible-fn ep-state-hyp)
                       (fn [hyp] (:pid hyp)) nil))]
    (add-hyp workspace hyp)))

(defn add-uninformed-mark-impossible-hyp
  [workspace ep-state-hyp problem ep-state-tree sensors params lazy]
  (let [no-explainers (find-no-explainers (:workspace (current-ep-state ep-state-tree)))
        prev-ep (previous-ep-state ep-state-tree)]
    (if (or (empty? no-explainers) (nil? prev-ep)) workspace
        (add-branch-hyp workspace ep-state-hyp prev-ep problem ep-state-tree
                        sensors params lazy))))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  (let [apriori (measure-decision-confidence (:workspace (:ep-state (:data ep-state-hyp))))
        hyp (new-hyp "MHA" :meta-accurate apriori
                     [(:id ep-state-hyp)] (constantly [])
                     (partial impossible-fn ep-state-hyp)
                     (constantly "Decision is accurate") nil)]
    (add-hyp workspace hyp)))

(defn generate-meta-hypotheses
  [workspace problem ep-state-tree sensors params lazy]
  (let [ep-state (current-ep-state ep-state-tree)
        ep-state-hyp (generate-ep-state-hyp ep-state)]
    (-> workspace
        (add-hyp ep-state-hyp)
        (force-acceptance ep-state-hyp)
        (add-accurate-decision-hyp ep-state-hyp)
        (add-uninformed-mark-impossible-hyp ep-state-hyp problem ep-state-tree
                                            sensors params lazy))))
