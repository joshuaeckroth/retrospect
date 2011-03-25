(ns retrospect.meta.hypotheses
  (:use [retrospect.epistemicstates :only
         [flatten-ep-state-tree current-ep-state goto-ep-state explain
          new-branch-ep-state left-ep-state previous-ep-state update-ep-state-tree
          new-child-ep-state]])
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

(defn find-no-explainers
  [workspace]
  (let [unexplained (ws/find-unexplained workspace)]
    (filter #(empty? (ws/find-explainers workspace % :static)) unexplained)))

(defn mark-least-conf-impossible
  "Given an ep-state (to go back to), mark the least confident
   accepted hypothesis as IMPOSSIBLE after clearing the decision and
   resetting confidences to their apriori values. The idea is to
   ensure that the least confident (and therefore, hopefully the most
   likely incorrect) hypothesis is not accepted again, and something
   else is accepted instead."
  [ep-state least-conf]
  (assoc ep-state :workspace
         (-> (:workspace ep-state)
             (ws/reset-confidences)
             (ws/prepare-workspace)
             (ws/reject-many [least-conf]))))

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
        ep-state (current-ep-state est)
        ep (if least-conf
             (mark-least-conf-impossible ep-state least-conf)
             (mark-many-impossible ep-state hyps))
        ep-expl-cycles (update-in ep [:workspace :resources] assoc :explain-cycles 0)]
    (comment (println "branch-mark-imp rejected:"
                      (map :id (:rejected (:workspace ep-expl-cycles)))))
    (update-ep-state-tree est ep-expl-cycles)))

(defn score-by-replaying
  [problem ep-state-tree sensors params lazy]
  (let [time-now (apply max (map :sensed-up-to sensors))]
    (loop [est ep-state-tree]
      (comment
        (println "hyps:" (map :id (ws/get-hyps (:workspace (current-ep-state est)))))
        (println "replay rejected @" (:time (current-ep-state est)) ":"
                 (map :id (:rejected (:workspace (current-ep-state est)))))
        (println "replay accepted @" (:time (current-ep-state est)) ":"
                 (map #(format "%s %s" (:id %)
                               (ws/hyp-conf (:workspace (current-ep-state est)) %))
                      (:accepted (:workspace (current-ep-state est)))))
        (println "replay rejected @" (:time (current-ep-state est)) ":"
                 (map :id (:rejected (:workspace (current-ep-state est)))))
        (println "replay unexplained:"
                 (map :id (ws/find-unexplained (:workspace (current-ep-state est))))))
      (if (< (:time (current-ep-state est)) time-now)
        (let [ep-state (current-ep-state est)
              est-child (new-child-ep-state est ep-state (:time ep-state) problem)
              ep-child (current-ep-state est-child)
              ep-hyps ((:hypothesize-fn problem) ep-child sensors (:time ep-child) params)
              ep-expl (explain ep-hyps)]
          (recur (update-ep-state-tree est-child ep-expl)))
        {:ep-state-tree est
         :explain-cycles (:explain-cycles (:resources (:workspace (current-ep-state est))))
         :score (ws/get-conf (:workspace (current-ep-state est)))}))))

(defn generate-ep-state-hyp
  [ep-state]
  (ws/new-hyp "HEP" :meta-ep nil NEUTRAL "ep-state hyp" {:ep-state ep-state}))

(defn add-branch-hyp
  [workspace ep-state-hyp branchable hyps problem ep-state-tree
   sensors params lazy prefix least-conf?]
  (let [lconf (if least-conf?
                (let [ws (:workspace (current-ep-state ep-state-tree))
                      hs (if (not-empty hyps) hyps (:accepted ws))]
                  (first (reverse (ws/sort-by-conf ws hs)))))
        est (branch-and-mark-impossible ep-state-tree branchable hyps lconf)
        ep-state (current-ep-state est)
        ;; bypass epistemicstate's explain, go directly to workspace's explain,
        ;; so that we don't cause another prepare-workspace before explaining
        ep-expl (assoc ep-state :workspace (ws/explain (:workspace ep-state)))
        est-new (update-ep-state-tree est ep-expl)
        hyp (let [{score :score est-replayed :ep-state-tree ec :explain-cycles}
                  (score-by-replaying problem est-new sensors params lazy)]
              (ws/new-hyp prefix :meta :meta score
                          (format "Marked impossible: %s"
                                  (if lconf (:id lconf)
                                      (apply str (interpose ", " (map :id hyps)))))
                          {:ep-state-tree est-replayed :explain-cycles ec}))]
    (comment
      (println "a-b-h ep-state rejected:" (map :id (:rejected (:workspace ep-state))))
      (println "a-b-h ep-expl rejected:" (map :id (:rejected (:workspace ep-expl)))))
    (ws/add workspace hyp [ep-state-hyp])))

(defn add-uninformed-mark-impossible-hyp
  [workspace ep-state-hyp problem ep-state-tree sensors params lazy]
  (let [prev-ep (previous-ep-state ep-state-tree)
        no-explainers (find-no-explainers (:workspace (current-ep-state ep-state-tree)))]
    (comment (println "no explainers:" (map :id no-explainers)))
    (if (or (nil? prev-ep) (empty? no-explainers)) workspace
        (let [ws (:workspace prev-ep)
              hyps (:accepted ws)]
          (add-branch-hyp
           workspace ep-state-hyp prev-ep hyps problem ep-state-tree
           sensors params lazy (format "H:U:%s:L" (:id prev-ep)) true)))))

(defn add-mark-impossible-hyp
  [workspace ep-state-hyp problem ep-state-tree sensors params lazy]
  (let [ep-state (current-ep-state ep-state-tree)
        rejectors (find-rejectors (:workspace ep-state))]
    (comment (println "rejectors:" (map :id rejectors)))
    (if (empty? rejectors) workspace
        (add-branch-hyp
         workspace ep-state-hyp ep-state rejectors problem ep-state-tree
         sensors params lazy (format "H:R:%s:*" (:id ep-state)) false))))

(defn add-mark-impossible-hyp-least-conf
  [workspace ep-state-hyp problem ep-state-tree branchable sensors params lazy]
  (let [ws (:workspace branchable)
        hyps (:accepted ws)]
    (add-branch-hyp
     workspace ep-state-hyp branchable hyps problem ep-state-tree
     sensors params lazy (format "H:LC:%s:L" (:id branchable)) true)))

(defn add-accurate-decision-hyp
  [workspace ep-state-hyp]
  (let [apriori (boost (ws/get-conf (:workspace (:ep-state (:data ep-state-hyp)))))
        hyp (ws/new-hyp "HA" :meta-accurate :meta apriori "Decision is accurate" nil)]
    (ws/add workspace hyp [ep-state-hyp])))

(defn generate-meta-hypotheses
  [workspace problem ep-state-tree sensors params lazy]
  (let [ep-state (current-ep-state ep-state-tree)
        ep-state-hyp (generate-ep-state-hyp ep-state)
        prev-ep (previous-ep-state ep-state-tree)
        ws (-> workspace
               (ws/add ep-state-hyp [])
               (ws/forced ep-state-hyp)
               (add-accurate-decision-hyp ep-state-hyp)
               (add-uninformed-mark-impossible-hyp
                ep-state-hyp problem ep-state-tree sensors params lazy)
               (add-mark-impossible-hyp
                ep-state-hyp problem ep-state-tree sensors params lazy))]
    (loop [ws2 ws
           states (take 5 (reverse (flatten-ep-state-tree ep-state-tree)))]
      (if (or (empty? states) (have-enough-meta-hyps ws2)) ws2
          (recur (add-mark-impossible-hyp-least-conf ws2
                  ep-state-hyp problem ep-state-tree (first states) sensors params lazy)
                 (rest states))))))
