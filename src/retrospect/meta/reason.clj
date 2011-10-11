(ns retrospect.meta.reason
  "
## Metareasoning in retrospect

Metareasoning in terms of retrospect is tasked with detecting if a
reasoning mistake has been committed, choosing what will be done about
it, and ultimately returning an alternative reasoning history (which
may include a new branch from some prior epistemic state). The
parameter :MetaStrategy chooses which strategy will be enacted.

The parameter :MetaStrategy can identify one of the following
metareasoning strategies:

1. no metareasoning; the given epistemic state tree is simply returned

2. batch from beginning: reprocess the whole history of inputs

3. domain-informed: ask the domain (\"problem\") to decide which
   ep-state to branch from

4. change acceptance threshold: reduce the threshold for acceptance in
   the current ep-state

5. gradual: first try changing acceptance threshold; if \"no
   good\" (defined below), then ask the domain for a branch point; if
   still \"no good,\" batch from beginning

### When is metareasoning activated?

Metareasoning should only be activated when there is \"sign of trouble\"
in the regular reasoning process. Because the regular, first-level
reasoning process is abductive, we can use the following metareasoning
activation conditions:

1. Some data (sensor reports) are unexplained

2. The resulting epistemic state has very high doubt

3. The prior 'n' epistemic states have somewhat high doubt ('n' is
   configurable by the parameter :DoubtBuildup)

If any of those conditions are met, metareasoning is activated.

When the \"gradual\" metareasoning strategy is active, each strategy is
considered, in order, if the prior strategy is \"no good.\" We say a
strategy is \"no good\" if any of the metareasoning activation
conditions are met after the strategy has done its work.
"
  (:use [retrospect.epistemicstates :only
         [previous-ep-state current-ep-state new-child-ep-state
          new-branch-ep-state new-branch-root explain goto-ep-state
          update-ep-state-tree nth-previous-ep-state]])
  (:use [retrospect.onerun :only
         [proceed-one-run-state update-one-run-state]])
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.state]))


(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [or-state]
  (if (:MetaReasoning params)
    (let [ep-state (current-ep-state (:ep-state-tree or-state))
          workspace (:workspace ep-state)]
      ;; TODO: implement other conditions
      (or (not-empty (:no-explainers (:final (:log workspace))))
          (< 0.10 (ws/get-unexplained-pct workspace))
          (< 0.10 (ws/get-doubt workspace))))))

(defn workspace-compare
  [ws1 ws2]
  (let [comp-unexp (compare (ws/get-unexplained-pct ws1)
                            (ws/get-unexplained-pct ws2))]
    (if (not= 0 comp-unexp) comp-unexp
        (compare (ws/get-doubt ws1) (ws/get-doubt ws2)))))

(defn batch
  [n or-state]
  (if-not
      ;; skip all of this if we are just an ep-state off the root
      (previous-ep-state (:ep-state-tree or-state)) or-state
      ;; otherwise, we're not straight out of the root, so do the batching
      (let [prior-est (:ep-state-tree or-state)
            prior-ep (current-ep-state prior-est)
            ;; branch back n if n != nil, otherwise branch from root
            est (if n (new-branch-ep-state prior-est (nth-previous-ep-state prior-est n))
                    (new-branch-root prior-est (:original-problem-data or-state)))
            ep-state (current-ep-state est)
            time-now (apply max (map :sensed-up-to (:sensors or-state)))
            [ep-hyps resources] ((:hypothesize-fn @problem) ep-state (:sensors or-state) time-now)
            ep-expl (explain ep-hyps (:problem-data or-state))
            est-expl (update-ep-state-tree est ep-expl)
            ors (assoc or-state :ep-state-tree est-expl)
            final-ors (if (> 0 (workspace-compare (:workspace ep-expl) (:workspace prior-ep)))
                        ;; if new workspace is better, stick with this
                        ;; (newer) branch, but also add on original
                        ;; explain cycles and hypothesis count and
                        ;; other resources
                        (-> (assoc ors :ep-state ep-expl)
                            (update-in [:resources :meta-accepted] inc)
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
  [or-state]
  or-state)

(defn lower-threshold
  [or-state]
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
                (recur (ws/explain workspace (:problem-data new-ep))
                       (- threshold 25))))
        final-or-state (update-one-run-state
                         (assoc or-state :ep-state-tree new-est)
                         (assoc new-ep :workspace final-ws)
                         {:compute 0 :memory 0})]
    (update-in final-or-state [:resources :meta-activations] inc)))

(defn gradual
  [or-state]
  (loop [ors or-state
         attempt [:BatchBeginning :LowerThreshold]]
    (if (or (not (metareasoning-activated? ors))
            (empty? attempt)) ors
      (cond (= (first attempt) :LowerThreshold)
            (recur (lower-threshold ors)
                   (rest attempt))
            (= (first attempt) :BatchBeginning)
            (recur (batch nil ors)
                   (rest attempt))))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :MetaStrategy)"
  [or-state]
  (batch 3 or-state))

