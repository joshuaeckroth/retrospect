(ns retrospect.meta.reason
  (:import (misc AlphanumComparator))
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only
         [previous-ep-state current-ep-state new-child-ep-state
          new-branch-ep-state new-branch-root explain goto-ep-state
          update-ep-state-tree nth-previous-ep-state ep-state-depth
          find-dependents retract-dependents]])
  (:use [retrospect.onerun :only
         [proceed-one-run-state update-one-run-state]])
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.state]))

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [or-state]
  (if (not= "NoMetaReasoning" (:MetaReasoning params))
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

(defn apply-and-evaluate
  [or-state prior-ep est]
  (let [ep-state (current-ep-state est)
        time-now (apply max (map :sensed-up-to (:sensors or-state)))
        [ep-hyps resources] ((:hypothesize-fn @problem) ep-state (:sensors or-state) time-now)
        ep-expl (explain ep-hyps time-now)
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
    (update-in final-ors [:resources :meta-activations] inc)))

(defn batch
  [n or-state]
  (if-not
      ;; skip all of this if we are just an ep-state off the root
      (previous-ep-state (:ep-state-tree or-state)) or-state
      ;; otherwise, we're not straight out of the root, so do the batching
      (let [prior-est (:ep-state-tree or-state)
            prior-ep (current-ep-state prior-est)
            ;; branch back n if n != nil and there are more than n states;
            ;; otherwise branch from root
            est (if (and n (< n (dec (ep-state-depth prior-est))))
                  (new-branch-ep-state prior-est (nth-previous-ep-state prior-est n)
                                       true true)
                  (new-branch-root prior-est (:original-problem-data or-state)))]
        (apply-and-evaluate or-state prior-ep est))))

(defn retract-no-explainers
  [or-state]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        no-explainers (:no-explainers (:final (:log (:workspace ep-state))))
        hyps ((:no-explainer-hyps-fn @problem) no-explainers (:problem-data ep-state))
        deps (find-dependents ep-state hyps)
        est (new-branch-ep-state (:ep-state-tree or-state) ep-state false false)
        ep-state-retracted (assoc (retract-dependents (current-ep-state est) deps)
                             :workspace (ws/init-workspace))
        est-retracted (update-ep-state-tree est ep-state-retracted)
        or-state-log (update-in or-state [:meta-logs (:id ep-state)]
                                str "Performing RetractNoExplainers..."
                                "\n\nHyps with no explainers: "
                                (apply str (interpose ", " (sort (AlphanumComparator.)
                                                                 (map :id no-explainers))))
                                "\n\nImplicated accepted hyps: "
                                (apply str (interpose ", " (sort (AlphanumComparator.)
                                                                 (map :id hyps))))
                                "\n\nRetracted: "
                                (apply str (interpose ", " (sort (AlphanumComparator.)
                                                                 (map :id deps))))
                                "\n\n")]
    (if (empty? hyps) or-state-log
        (apply-and-evaluate or-state-log ep-state est-retracted))))

(defn lower-threshold
  [or-state]
  (if (= 0 (:Threshold params)) or-state
      (let [est (:ep-state-tree or-state)
            ep-state (current-ep-state est)
            new-est (if (= 1 (ep-state-depth est))
                      (new-branch-root est (:original-problem-data or-state))
                      (new-branch-ep-state est ep-state true true))
            new-ep (current-ep-state new-est)]
        ;; drop threshold to 0
        (binding [params (assoc params :Threshold 0)]
          (apply-and-evaluate or-state ep-state new-est)))))

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
   the parameter :MetaReasoning)"
  [or-state]
  (cond (= "BatchBeginning" (:MetaReasoning params))
        (batch nil or-state)
        (= "Batch1" (:MetaReasoning params))
        (batch 1 or-state)
        (= "Batch2" (:MetaReasoning params))
        (batch 2 or-state)
        (= "Batch3" (:MetaReasoning params))
        (batch 3 or-state)
        (= "Batch4" (:MetaReasoning params))
        (batch 4 or-state)
        (= "Batch5" (:MetaReasoning params))
        (batch 5 or-state)
        (= "RetractNoExplainers" (:MetaReasoning params))
        (retract-no-explainers or-state)
        (= "LowerThreshold" (:MetaReasoning params))
        (lower-threshold or-state)
        :else or-state))