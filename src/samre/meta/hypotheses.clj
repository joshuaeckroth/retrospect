(ns samre.meta.hypotheses
  (:use [samre.workspaces :only
         [new-hyp add-hyp measure-decision-confidence force-acceptance
          clear-decision reset-confidences-to-apriori update-hyps
          lookup-hyps update-candidates-unexplained]])
  (:use [samre.epistemicstates :only
         [flatten-ep-state-tree current-ep-state explain
          new-branch-ep-state left-ep-state previous-ep-state update-ep-state-tree]])
  (:use [samre.meta.meta :only [have-enough-meta-hyps]])
  (:use [samre.confidences]))

(defn mark-least-conf-impossible
  "Given an ep-state (to go back to), mark the least confident
   accepted hypothesis as IMPOSSIBLE after clearing the decision and
   resetting confidences to their apriori values. The idea is to
   ensure that the least confident (and therefore, hopefully the most
   likely incorrect) hypothesis is not accepted again, and something
   else is accepted instead."
  [ep-state]
  (comment (println "pre-mark: " (str ep-state) (:decision (:workspace ep-state))
                    (map :id (vals (:hyps (:workspace ep-state))))))
  (if (empty? (:accepted (:decision (:workspace ep-state))))
    (do (comment (println "empty decision: " (str ep-state))) ep-state)
    (let [ws (:workspace ep-state)
          least-conf (first (sort-by :confidence (lookup-hyps ws (:accepted (:decision ws)))))
          least-conf-impossible (assoc least-conf :confidence IMPOSSIBLE)]
      (comment (println "least-conf nil? " (nil? least-conf) "-" (str ep-state)))
      (assoc ep-state :workspace
             (-> ws
                 (reset-confidences-to-apriori)
                 (clear-decision)
                 (update-hyps [least-conf-impossible])
                 (update-candidates-unexplained))))))

(defn branch-and-mark-impossible
  "A composite action that branches at the specified ep-state,
   then marks the least confident accepted hyp as IMPOSSIBLE and
   updates the OneRunState with all these changes."
  [ep-state-tree ep-state]
  (comment (println "pre-branch: " (str ep-state) (:decision (:workspace ep-state))
                    (map :id (vals (:hyps (:workspace ep-state))))))
  (let [est (new-branch-ep-state ep-state-tree ep-state)
        ep (mark-least-conf-impossible (current-ep-state est))]
    (comment (println "post-branch: " (str ep) (:decision (:workspace ep)))
             (map :id (vals (:hyps (:workspace ep)))))
    (update-ep-state-tree est ep)))

(defn score-by-explaining
  [problem ep-state-tree sensors params lazy]
  (let [ep-state (current-ep-state ep-state-tree)]
    (comment (println "pre-score: " (str ep-state) (:decision (:workspace ep-state))
                      (map :id (vals (:hyps (:workspace ep-state)))))))
  (let [time-prev (if-let [time-prev (:time (previous-ep-state ep-state-tree))]
                    (inc time-prev) (:time (current-ep-state ep-state-tree)))
        time-now (apply max (map :sensed-up-to sensors))
        ep-state (current-ep-state ep-state-tree)
        #_(generate-hyps-and-explain problem (current-ep-state ep-state-tree)
                                     time-prev time-now sensors params lazy)
        score (measure-decision-confidence (:workspace ep-state))
        ep-state-updated-time (assoc ep-state :time time-now)]
    {:score score :ep-state ep-state-updated-time}))

(defn generate-ep-state-hyp
  [ep-state]
  (new-hyp "EP" :meta VERY-PLAUSIBLE [] (constantly []) (constantly [])
           (constantly "ep-state" {:ep-state ep-state})))

(defn impossible-fn
  [ep-state-hyp hyp hyps]
  (filter (fn [h] (and (not= (:id hyp) (:id h))
                       (some #(= % (:id ep-state-hyp)) (:explains h)))) hyps))

(defn add-branch-hyp
  [workspace ep-state-hyp branchable problem ep-state-tree sensors params lazy]
  (let [est (branch-and-mark-impossible ep-state-tree branchable)
        hyp (let [{score :score ep :ep-state}
                  (score-by-explaining problem est sensors params lazy)]
              (new-hyp "MH" :meta score [(:id ep-state-hyp)]
                       (constantly []) (partial impossible-fn ep-state-hyp)
                       (fn [hyp] (:pid hyp))
                       {:ep-state-tree est :ep-state ep}))]
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
        hyp (let [{score :score ep :ep-state}
                  (score-by-explaining problem est2 sensors params true)]
              (new-hyp "MH+" :meta score [(:id ep-state-hyp)] (constantly [])
                       (partial impossible-fn ep-state-hyp)
                       (fn [hyp] (:pid hyp)) nil))]
    (add-hyp workspace hyp)))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  (let [apriori (measure-decision-confidence (:workspace (:ep-state (:data ep-state-hyp))))
        hyp (new-hyp "MHA" :meta apriori
                     [(:id ep-state-hyp)] (constantly [])
                     (partial impossible-fn ep-state-hyp)
                     (constantly "Decision is accurate") nil)]
    (add-hyp workspace hyp)))

(defn generate-meta-hypotheses
  [workspace problem ep-state-tree sensors params lazy]
  (let [ep-state-hyp (generate-ep-state-hyp (current-ep-state ep-state-tree))
        ws (-> workspace
               (add-hyp ep-state-hyp)
               (force-acceptance ep-state-hyp)
               (add-accurate-decision-hyp ep-state-hyp))
        ws2 (if lazy (add-more-explainers-hyp ws ep-state-hyp problem
                                              ep-state-tree sensors params)
                ws)]
    (loop [ws ws2
           branchable (flatten-ep-state-tree ep-state-tree)]
      (if (or (empty? branchable) (and lazy (have-enough-meta-hyps (vals (:hyps ws))))) ws
          (recur (add-branch-hyp ws ep-state-hyp (first branchable) problem
                                 ep-state-tree sensors params lazy)
                 (rest branchable))))))
