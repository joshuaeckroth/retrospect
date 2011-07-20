(ns retrospect.meta.reason
  (:use [retrospect.epistemicstates :only
         [previous-ep-state current-ep-state new-child-ep-state
          new-branch-ep-state]])
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

(def meta-strategies [:BatchBeginning :DomainInformed :LowerThreshold :Gradual])

(defn batch-from-beginning
  [problem or-state params]
  or-state)

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
        (loop [workspace (:workspace ep-state)]
          (cond (or (empty? (:unaccepted (:final (:log workspace))))
                    (= 1.0 (:threshold workspace)))
                workspace
                (not= (:accepted (:final (:log (:workspace ep-state))))
                      (:accepted (:final (:log workspace))))
                workspace
                :else
                (recur (-> workspace (ws/lower-threshold)
                         (ws/explain (:inconsistent-fn problem)
                                     (:problem-data new-ep))))))] 
    ;; if resulting workspace is no different, return original or-state
    (if (= (:accepted (:final (:log (:workspace ep-state))))
           (:accepted (:final (:log final-ws))))
      or-state
      (update-in (update-one-run-state (assoc or-state :ep-state-tree new-est)
                                       (assoc new-ep :workspace final-ws))
                 [:resources :meta-activations] inc))))

(defn gradual
  [problem or-state params]
  or-state)

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [or-state params]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))]
    ;; TODO: implement other conditions
    (or (not-empty (:unexplained (:final (:log (:workspace ep-state))))))))

(defn metareason
  "Activate the appropriate metareasoning strategy (as given by
   the parameter :MetaStrategy)"
  [problem or-state params]
  (let [strat (:meta-strategy or-state)]
    (cond (= :NoMetareasoning strat) or-state
          (= :BatchBeginning strat) (batch-from-beginning problem or-state params)
          (= :DomainInformed strat) (domain-informed problem or-state params)
          (= :LowerThreshold strat) (lower-threshold problem or-state params)
          (= :Gradual strat) (gradual problem or-state params)
          :else or-state)))

