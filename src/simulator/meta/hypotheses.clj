(ns simulator.meta.hypotheses
  (:require [simulator workspaces])
  (:import [simulator.workspaces Hypothesis])
  (:use [simulator.workspaces :only
         [add-hyp measure-decision-confidence force-acceptance
          clear-decision reset-confidences-to-apriori update-hyps
          lookup-hyps update-candidates-unexplained]])
  (:use [simulator.epistemicstates :only
         [flatten-ep-state-tree current-ep-state generate-hyps-and-explain
          new-branch-ep-state update-ep-state-tree left-ep-state previous-ep-state]])
  (:use [simulator.confidences]))

(defrecord EpistemicStateHypothesis [ep-state])

(defrecord MetaHypothesis [id apriori action])

(defn mark-least-conf-impossible
  "Given an ep-state (to go back to), mark the least confident
   accepted hypothesis as IMPOSSIBLE after clearing the decision and
   resetting confidences to their apriori values. The idea is to
   ensure that the least confident (and therefore, hopefully the most
   likely incorrect) hypothesis is not accepted again, and something
   else is accepted instead."
  [ep-state]
  (comment
    (println "pre-mark: " (str ep-state) (:decision (:workspace ep-state))
             (map :id (vals (:hyps (:workspace ep-state)))))
    (if (empty? (:accepted (:decision (:workspace ep-state))))
      (do (println "empty decision: " (str ep-state)) ep-state)
      (let [ws (:workspace ep-state)
            least-conf (first (sort-by :confidence (lookup-hyps ws (:accepted (:decision ws)))))
            least-conf-impossible (assoc least-conf :confidence IMPOSSIBLE)]
        (println "least-conf nil? " (nil? least-conf) "-" (str ep-state))
        (assoc ep-state :workspace
               (-> ws
                   (reset-confidences-to-apriori)
                   (clear-decision)
                   (update-hyps [least-conf-impossible])
                   (update-candidates-unexplained))))))
  ep-state)

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
        ep-state-prepared ((:prepare-hyps-fn problem) (current-ep-state ep-state-tree)
                           time-prev sensors params)
        ep-state (generate-hyps-and-explain problem ep-state-prepared sensors params lazy)
        score (measure-decision-confidence (:workspace ep-state))
        ep-state-updated-time (assoc ep-state :time (apply max (map :sensed-up-to sensors)))]
    {:score score :ep-state ep-state-updated-time}))

(defn generate-ep-state-hyp
  [ep-state]
  (Hypothesis. :ep-state :meta VERY-PLAUSIBLE VERY-PLAUSIBLE
               [] (constantly []) (constantly []) identity
               (constantly false)
               (constantly "ep-state") nil))

(defn impossible-fn
  [ep-state-hyp hyp hyps]
  (filter (fn [e] (some #(= % (:id ep-state-hyp)) (:explains e))) hyps))

;; TODO: remove (take 3 (shuffle #))
(defn add-revisit-ep-state-hyps
  [workspace ep-state-hyp problem ep-state-tree sensors params lazy]
  "For all existing ep-states, attempt to revisit them and apply each
   of many possible actions to take on that ep-state (e.g., mark
   IMPOSSIBLE least-confident hyp)."
  (let [ep-states (flatten-ep-state-tree ep-state-tree)
        ests (take 3 (shuffle (map (partial branch-and-mark-impossible ep-state-tree)
                                   ep-states)))
        make-hyp (fn [est] (let [{score :score ep :ep-state}
                                 (score-by-explaining problem est sensors params lazy)]
                             (Hypothesis. (keyword
                                           (format "MH%d" (hash [ep-state-hyp est ep score])))
                                          :meta
                                          score score
                                          [(:id ep-state-hyp)] (constantly [])
                                          (partial impossible-fn ep-state-hyp)
                                          (constantly (update-ep-state-tree est ep))
                                          (fn [_ time] (not= time (:time ep-state-hyp)))
                                          (fn [hyp] (name (:id hyp)))
                                          nil)))
        hyps (map make-hyp ests)]
    (reduce add-hyp workspace hyps)))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  (let [apriori (measure-decision-confidence (:workspace (:ep-state ep-state-hyp)))
        hyp (Hypothesis. :MH-dec-accurate :meta apriori apriori
                         [(:id ep-state-hyp)] (constantly [])
                         (partial impossible-fn ep-state-hyp)
                         identity (constantly false)
                         (constantly "Decision is accurate") nil)]
    (add-hyp workspace hyp)))

(defn generate-meta-hypotheses
  [workspace problem ep-state-tree sensors params lazy]
  (let [ep-state-hyp (generate-ep-state-hyp (current-ep-state ep-state-tree))]
    (-> workspace
        (add-hyp ep-state-hyp)
        (force-acceptance ep-state-hyp)
        (add-accurate-decision-hyp ep-state-hyp)
        (add-revisit-ep-state-hyps ep-state-hyp problem ep-state-tree sensors params lazy))))
