(ns retrospect.meta.robustness
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only
         [current-ep-state previous-ep-state new-branch-ep-state
          new-branch-root ep-state-depth explain]])
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.state]))

(defn analyze-sensitivity
  [or-state truedata]
  (let [est (:ep-state-tree or-state)
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
        hyps-same (set (filter (fn [h] (some (fn [h2] ((:hyps-equal?-fn @problem) h h2))
                                             hyps2)) hyps))
        true-same (filter true-hyps hyps-same)
        false-same (filter false-hyps hyps-same)]
    {:AvgTrueSensitivity (cond (empty? true-hyps) 0.0
                               (empty? true-same) 1.0
                               :else
                               (- 1.0 (double (/ (count true-same)
                                                 (count true-hyps)))))
     :AvgFalseSensitivity (cond (empty? false-hyps) 0.0
                                (empty? false-same) 1.0
                                :else
                                (- 1.0 (double (/ (count false-same)
                                                  (- (count hyps) (count true-hyps))))))}))
