(ns retrospect.meta.reason
  (:use [retrospect.epistemicstates :only
         [previous-ep-state current-ep-state new-child-ep-state
          new-branch-ep-state new-branch-root explain goto-ep-state
          update-ep-state-tree]])
  (:use [retrospect.onerun :only
         [proceed-one-run-state update-one-run-state]])
  (:require [retrospect.workspaces :as ws]))


;; ## Metareasoning in retrospect
;;
;; Metareasoning in terms of retrospect is tasked with detecting if a reasoning
;; mistake has been committed, choosing what will be done about it, and
;; ultimately returning an alternative reasoning history (which may include
;; a new branch from some prior epistemic state). The parameter :MetaStrategy
;; chooses which strategy will be enacted.
;;
;; The parameter :MetaStrategy can identify one of the following metareasoning
;; strategies:
;;
;; 1. no metareasoning; the given epistemic state tree is simply returned
;; 2. batch from beginning: reprocess the whole history of inputs
;; 3. domain-informed: ask the domain ("problem") to decide which ep-state to
;; branch from
;; 4. change acceptance threshold: reduce the threshold for acceptance in the
;; current ep-state
;; 5. gradual: first try changing acceptance threshold; if "no good" (defined
;; below), then ask the domain for a branch point; if still "no good," batch
;; from beginning
;;
;; ### When is metareasoning activated?
;;
;; Metareasoning should only be activated when there is "sign of trouble" in
;; the regular reasoning process. Because the regular, first-level reasoning
;; process is abductive, we can use the following metareasoning activation
;; conditions:
;;
;; 1. Some data (sensor reports) are unexplained
;; 2. The resulting epistemic state has very high doubt
;; 3. The prior 'n' epistemic states have somewhat high doubt ('n' is
;; configurable by the parameter :DoubtBuildup)
;;
;; If any of those conditions are met, metareasoning is activated.
;;
;; When the "gradual" metareasoning strategy is active, each strategy is
;; considered, in order, if the prior strategy is "no good." We say a strategy
;; is "no good" if any of the metareasoning activation conditions are met after
;; the strategy has done its work.

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [or-state params]
  (if (:meta or-state)
    (let [ep-state (current-ep-state (:ep-state-tree or-state))]
      ;; TODO: implement other conditions
      (or (not-empty (:no-explainers (:final (:log (:workspace ep-state)))))
          (< 0.15 (ws/get-doubt (:workspace ep-state)))))))

(defn batch-from-beginning
  [problem or-state params]
  (if-not
      ;; skip all of this if we are just an ep-state off the root
      (previous-ep-state (:ep-state-tree or-state)) or-state
      ;; otherwise, we're not straight out of the root, so do the batching
      (let [prior-est (:ep-state-tree or-state)
            prior-ep (current-ep-state prior-est)
            est (new-branch-root prior-est (:original-problem-data or-state))
            ep-state (current-ep-state est)
            time-now (apply max (map :sensed-up-to (:sensors or-state)))
            [ep-hyps resources] ((:hypothesize-fn problem) ep-state (:sensors or-state)
                                 time-now params)
            ep-expl (explain ep-hyps (:get-more-hyps-fn problem)
                             (:inconsistent-fn problem)
                             (:Threshold params) params)
            est-expl (update-ep-state-tree est ep-expl)
            ors (assoc or-state :ep-state-tree est-expl)
            final-ors (if (< (ws/get-doubt (:workspace ep-expl))
                             (ws/get-doubt (:workspace prior-ep)))
                        ;; if new doubt is lower, stick with this (newer) branch,
                        ;; but also add on original explain cycles
                        ;; and hypothesis count and other resources
                        (-> (assoc ors :ep-state ep-expl)
                            (update-in [:resources :explain-cycles]
                                       + (:explain-cycles (:resources (:workspace prior-ep))))
                            (update-in [:resources :hypothesis-count]
                                       + (:hypothesis-count (:resources (:workspace prior-ep))))
                            (update-in [:resources :compute] + (:compute resources))
                            (update-in [:resources :memory] + (:memory resources)))
                        ;; otherwise, go back to prior (original) branch
                        ;; but also update explain cycles and hypothesis count
                        ;; and other resources
                        (-> (assoc ors :ep-state-tree
                                   (goto-ep-state est-expl (:id prior-ep))
                                   :ep-state prior-ep)
                            (update-in [:resources :explain-cycles]
                                       + (:explain-cycles (:resources (:workspace ep-expl))))
                            (update-in [:resources :hypothesis-count]
                                       + (:hypothesis-count (:resources (:workspace ep-expl))))
                            (update-in [:resources :compute] + (:compute resources))
                            (update-in [:resources :memory] + (:memory resources))))]
        (update-in final-ors [:resources :meta-activations] inc))))

(defn domain-informed
  [problem or-state params]
  or-state)

(defn lower-threshold
  [problem or-state params]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        new-est (new-branch-ep-state (:ep-state-tree or-state) ep-state)
        new-ep (current-ep-state new-est)
        ;; repeatedly lower threshold until new stuff is accepted
        ;; or cannot lower it any more (or there's nothing left to accept)
        final-ws
        (loop [workspace (:workspace ep-state)
               threshold (- (:Threshold params) 25)]
          (cond (or (empty? (:unaccepted (:final (:log workspace))))
                    (> 0 threshold))
                workspace
                (< (ws/get-doubt workspace) (ws/get-doubt (:workspace ep-state)))
                workspace
                :else
                (recur (ws/explain workspace
                                   (:inconsistent-fn problem)
                                   (:problem-data new-ep)
                                   (double (/ threshold 100.0)) params)
                       (- threshold 25))))
        final-or-state (update-one-run-state
                         (assoc or-state :ep-state-tree new-est)
                         (assoc new-ep :workspace final-ws)
                         {:compute 0 :memory 0})]
    (update-in final-or-state [:resources :meta-activations] inc)))

(defn gradual
  [problem or-state params]
  (loop [ors or-state
         attempt [:BatchBeginning :LowerThreshold]]
    (if (or (not (metareasoning-activated? ors params))
            (empty? attempt)) ors
      (cond (= (first attempt) :LowerThreshold)
            (recur (lower-threshold problem ors params)
                   (rest attempt))
            (= (first attempt) :BatchBeginning)
            (recur (batch-from-beginning problem ors params)
                   (rest attempt))))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :MetaStrategy)"
  [problem or-state params]
  (batch-from-beginning problem or-state params))

