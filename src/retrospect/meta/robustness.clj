(ns retrospect.meta.robustness
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
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
  [or-state true-false]
  (let [hyps-equal? (:hyps-equal?-fn @problem)
        est (:ep-state-tree or-state)
        ep-state (current-ep-state est)
        prev-ep (previous-ep-state est)
        pdata (:problem-data ep-state)
        workspace (:workspace prev-ep)
        time (:time ep-state)
        ep-branch (current-ep-state
                   (if (= 2 (ep-state-depth est))
                     (new-branch-root est (:original-problem-data or-state))
                     (new-branch-ep-state est prev-ep true true)))
        perturbed-sensors (map (:perturb-fn @problem) (:sensors or-state))
        [ep-hyps _] ((:hypothesize-fn @problem) ep-branch perturbed-sensors time)
        ep-expl (explain ep-hyps time)
        workspace2 (:workspace ep-expl)
        hyps2 (set/difference (ws/get-hyps workspace2 :static) (:forced workspace2))
        calc-sensitivity (fn [subtype tf]
                           (map (fn [h]
                                  (let [h-same (find-first (fn [h2] (hyps-equal? h h2))
                                                           hyps2)]
                                    (if-not h-same 1.0
                                            (Math/abs
                                             (- (ws/hyp-conf workspace h)
                                                (ws/hyp-conf workspace2 h-same))))))
                                (get (get true-false subtype) tf)))
        true-false-sensitivity (reduce (fn [m subtype]
                                         (assoc m subtype
                                                {true (calc-sensitivity subtype true)
                                                 false (calc-sensitivity subtype false)}))
                                       {} (keys true-false))
        avg (fn [xs] (if (empty? xs) 0.0 (double (/ (reduce + xs) (count xs)))))]
    (reduce (fn [m subtype]
              (let [k (apply str (map str/capitalize (str/split (name subtype) #"-")))]
                (assoc m
                  (keyword (format "AvgTrueSensitivity%s" k))
                  (avg (get (get true-false-sensitivity subtype) true))
                  (keyword (format "AvgFalseSensitivity%s" k))
                  (avg (get (get true-false-sensitivity subtype) false)))))
            {} (keys true-false-sensitivity))))

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
