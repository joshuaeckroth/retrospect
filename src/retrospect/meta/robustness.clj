(ns retrospect.meta.robustness
  (:require [clojure.set :as set])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [loom.graph :only [nodes]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state new-branch-ep-state
          new-branch-root ep-state-depth explain]])
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.state]))

(defn analyze-sensitivity
  [or-state truedata]
  (let [hyps-equal? (:hyps-equal?-fn @problem)
        est (:ep-state-tree or-state)
        ep-state (current-ep-state est)
        prev-ep (previous-ep-state est)
        pdata (:problem-data ep-state)
        workspace (:workspace prev-ep)
        time (:time ep-state)
        hyps (set/difference (ws/get-hyps workspace :static) (:forced workspace))
        true-hyps (set (filter (partial (:true-hyp?-fn @problem) truedata pdata time) hyps))
        false-hyps (set/difference hyps true-hyps)
        ep-branch (current-ep-state
                   (if (= 2 (ep-state-depth est))
                     (new-branch-root est (:original-problem-data or-state))
                     (new-branch-ep-state est prev-ep true true)))
        perturbed-sensors (map (:perturb-fn @problem) (:sensors or-state))
        [ep-hyps _] ((:hypothesize-fn @problem) ep-branch perturbed-sensors time)
        ep-expl (explain ep-hyps time)
        workspace2 (:workspace ep-expl)
        hyps2 (set/difference (ws/get-hyps workspace2 :static) (:forced workspace2))
        hyps-same (map (fn [h] (let [h-same (find-first
                                             (fn [h2] (hyps-equal? h h2)) hyps2)]
                                 (if h-same
                                   [h (Math/abs (- (ws/hyp-conf workspace h)
                                                   (ws/hyp-conf workspace2 h-same)))])))
                       hyps)
        true-same (filter (comp true-hyps first) (filter identity hyps-same))
        false-same (filter (comp false-hyps first) (filter identity hyps-same))]
    {:AvgTrueSensitivity (if (empty? true-same) 0.0
                             (/ (reduce + (map second true-same)) (count true-same)))
     :CountTrueSame (count true-same)
     :AvgFalseSensitivity (if (empty? false-same) 0.0
                              (/ (reduce + (map second false-same)) (count false-same)))
     :CountFalseSame (count false-same)}))

(defn analyze-dependency
  [or-state hyp starts]
  (let [ep-state (:ep-state or-state)
        depgraph (:depgraph ep-state)
        hyp-deps (set (pre-traverse depgraph hyp))
        consider (filter #(= (:type hyp) (:type %)) starts)
        common-deps (filter (comp not-empty second)
                            (map (fn [s] [s (set/intersection (set (pre-traverse depgraph s))
                                                              hyp-deps)])
                                 (filter #(not= hyp %) consider)))]
    common-deps))

(defn analyze-dependency-quick
  [or-state hyp starts]
  (let [ep-state (:ep-state or-state)
        depgraph (:depgraph ep-state)
        hyp-deps (set (pre-traverse depgraph hyp))
        consider (filter #(= (:type hyp) (:type %)) starts)]
    (filter (fn [s] (some hyp-deps (pre-traverse depgraph s)))
            (filter #(not= hyp %) consider))))
