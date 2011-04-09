(ns retrospect.meta.hypotheses
  (:import (misc AlphanumComparator))
  (:use [retrospect.epistemicstates :only
         [flatten-ep-state-tree current-ep-state goto-ep-state explain
          new-branch-ep-state new-child-ep-state previous-ep-state
          update-ep-state-tree]])
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.meta.meta :only [have-enough-meta-hyps]])
  (:use [retrospect.confidences])
  (:require [clojure.set :as set]))

(defn find-rejectors
  [workspace]
  (let [unexplained (ws/find-unexplained workspace)
        accepted (:accepted workspace)
        explainers (mapcat #(ws/find-explainers workspace % :static) unexplained)
        rejected-explainers (filter #(= IMPOSSIBLE (ws/hyp-conf workspace %)) explainers)]
    (filter (fn [hyp] (not-empty (set/intersection
                                  (set rejected-explainers)
                                  (ws/find-conflicts workspace hyp :static))))
            accepted)))

(defn mark-many-impossible
  [ep-state hyps]
  (let [ws (-> (:workspace ep-state)
               (ws/reset-confidences)
               (ws/prepare-workspace))]
    (assoc ep-state :workspace (ws/reject-many ws hyps))))

(defn branch-and-mark-impossible
  "A composite action that branches at the specified ep-state,
   then marks the least confident hyp in 'hyps' as IMPOSSIBLE and
   updates the OneRunState with all these changes."
  [ep-state-tree ep-state hyps least-conf]
  (let [est (new-branch-ep-state ep-state-tree ep-state)
        ;; get new branched ep-state
        ep-state (current-ep-state est)
        ep (if least-conf
             (mark-many-impossible ep-state [least-conf])
             (mark-many-impossible ep-state hyps))
        ep-expl-cycles (update-in ep [:workspace :resources] assoc :explain-cycles 0)]
    (update-ep-state-tree est ep-expl-cycles)))

(defn score-by-catching-up
  [problem ep-state-tree sensors params lazy]
  (let [time-now (apply max (map :sensed-up-to sensors))
        ep-state (current-ep-state ep-state-tree)
        est-child (new-child-ep-state ep-state-tree ep-state (dec time-now) problem)
        ep-child (current-ep-state est-child)
        ep-hyps ((:hypothesize-fn problem) ep-child sensors time-now params)
        ep-expl (explain ep-hyps)
        est-final (new-child-ep-state est-child ep-expl time-now problem)]
    {:ep-state-tree est-final
     :explain-cycles (:explain-cycles (:resources (:workspace ep-expl)))
     ;; penalize a result that has 'bad' hyps
     :score (if (not-empty (:bad (:problem-data (current-ep-state est-final))))
              (penalize (ws/get-conf (:workspace ep-expl)))
              (ws/get-conf (:workspace ep-expl)))}))

(defn add-branch-hyp
  [workspace ep-state-hyp branchable hyps problem ep-state-tree
   sensors params lazy prefix least-conf?]
  (let [lconf (if least-conf?
                (let [ws (:workspace branchable)
                      hs (if (not-empty hyps) hyps (:accepted ws))]
                  (first (reverse (ws/sort-by-conf ws hs)))))
        est (branch-and-mark-impossible ep-state-tree branchable hyps lconf)
        ;; get new branched ep-state
        ep-state (current-ep-state est)
        ;; bypass epistemicstate's explain, go directly to workspace's explain,
        ;; so that we don't cause another prepare-workspace before explaining
        ep-expl (assoc ep-state :workspace (ws/explain (:workspace ep-state)))
        est-new (update-ep-state-tree est ep-expl)
        hyp (let [{score :score est-caught-up :ep-state-tree ec :explain-cycles}
                  (score-by-catching-up problem est-new sensors params lazy)
                  rejected-hyps (sort-by :id (AlphanumComparator.) hyps)]
              (ws/new-hyp prefix :meta :meta score
                          (format "Marked impossible: %s"
                                  (if lconf (:id lconf)
                                      (apply str (interpose ", " (map :id rejected-hyps)))))
                          {:ep-state-tree est-caught-up :explain-cycles ec}))]
    (ws/add workspace hyp [ep-state-hyp])))

(defn add-mark-impossible-hyp
  [workspace ep-state-hyp problem ep-state-tree sensors params lazy]
  (let [ep-state (previous-ep-state ep-state-tree)
        rejectors (find-rejectors (:workspace ep-state))]
    (if (empty? rejectors) workspace
        (add-branch-hyp
         workspace ep-state-hyp ep-state rejectors problem ep-state-tree
         sensors params lazy (format "H:R:%s:*" (:id ep-state)) false))))

(defn add-mark-all-bad-hyp
  [workspace ep-state-hyp problem ep-state-tree sensors bad params lazy]
  (let [ep-state (previous-ep-state ep-state-tree)]
    (add-branch-hyp
     workspace ep-state-hyp ep-state bad problem ep-state-tree
     sensors params lazy (format "H:B:%s" (:id ep-state)) false)))

(defn add-mark-impossible-hyp-least-conf
  [workspace ep-state-hyp problem ep-state-tree branchable sensors params lazy]
  (let [ws (:workspace branchable)
        hyps (:accepted ws)]
    (if (empty? hyps) workspace
        (add-branch-hyp
         workspace ep-state-hyp branchable hyps problem ep-state-tree
         sensors params lazy (format "H:LC:%s:L" (:id branchable)) true))))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  ;; penalize ep-state's confidence since we're attempting meta-abduction
  ;; (so there was some 'bad' stuff that needs to be dealt with)
  (let [apriori (penalize (ws/get-conf (:workspace (:ep-state (:data ep-state-hyp)))))
        hyp (ws/new-hyp "HA" :meta-accurate :meta apriori "Decision is accurate" nil)]
    (ws/add workspace hyp [ep-state-hyp])))

(defn generate-ep-state-hyp
  [ep-state]
  (ws/new-hyp "HEP" :meta-ep nil NEUTRAL "ep-state hyp" {:ep-state ep-state}))

(defn generate-meta-hypotheses
  [workspace problem ep-state-tree sensors bad params lazy]
  (let [ep-state (previous-ep-state ep-state-tree)
        ep-state-hyp (generate-ep-state-hyp ep-state)
        ws (-> workspace
               (ws/add ep-state-hyp [])
               (ws/forced ep-state-hyp)
               (add-accurate-decision-hyp ep-state-hyp)
               (add-mark-impossible-hyp
                ep-state-hyp problem ep-state-tree sensors params lazy)
               (add-mark-all-bad-hyp
                ep-state-hyp problem ep-state-tree sensors bad params lazy))
        ;; use (butlast) here because we don't want to branch
        ;; the empty child ep-state from the non-meta reasoning cycle
        potential-branches (take 3 (reverse (butlast (flatten-ep-state-tree ep-state-tree))))]
    (loop [ws2 ws
           states potential-branches]
      (if (or (empty? states) (have-enough-meta-hyps ws2)) ws2
          (recur (add-mark-impossible-hyp-least-conf ws2
                  ep-state-hyp problem ep-state-tree (first states) sensors params lazy)
                 (rest states))))))
