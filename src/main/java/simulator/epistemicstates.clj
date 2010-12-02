(ns simulator.epistemicstates
  (:require [simulator logs])
  (:import [simulator.logs LogEntry AbducerLogEntry HypLogEntry])
  (:use [simulator.hypotheses :only
         (init-hypspace get-explainers get-hyp-id-str get-hyp-ids-str set-confidence
                        penalize boost find-conflicts find-unexplained get-confidence
                        add-explainers get-apriori add-conflicts find-essentials
                        find-best)])
  (:use [simulator.confidences])
  (:require [clojure.zip :as zip])
  (:require [clojure.set :as set])
  (:require [clojure.walk :as walk])
  (:require [vijual :as vijual]))

(defprotocol EpistemicStateTree
  (branch? [ep-state] "Is it possible for node to have children?")
  (node-children [ep-state] "Return children of this node.")
  (make-node [ep-state children] "Makes new node from existing node and new children."))

(defrecord EpistemicState
    [id
     children
     time
     hypspace
     accepted
     hypothesized
     unexplained
     decision
     log
     abducer-log
     hyp-log
     problem-data]
  Object
  (toString [_] (format "%s %d %s" id time (confidence-str (:confidence decision)))))

(defn clone-ep-state
  [ep-state id children]
  (EpistemicState.
   id
   children
   (:time ep-state)
   (:hypspace ep-state)
   (:accepted ep-state)
   (:hypothesized ep-state)
   (:unexplained ep-state)
   (:decision ep-state)
   (:log ep-state)
   (:abducer-log ep-state)
   (:hyp-log ep-state)
   (:problem-data ep-state)))

(extend-protocol EpistemicStateTree
  EpistemicState
  (branch? [ep-state] true)
  (node-children [ep-state] (seq (:children ep-state)))
  (make-node [ep-state children] (clone-ep-state ep-state (:id ep-state) children)))

(defn zip-ep-state-tree
  [root]
  (zip/zipper branch? node-children make-node root))

(defn make-ep-state-id
  ([]
     "A")
  ([ep-state-tree]
     (let [count
           (loop [count 1
                  loc (zip-ep-state-tree (zip/root ep-state-tree))]
             (if (zip/end? loc) count
                 (recur (inc count) (zip/next loc))))]
       (loop [i count
              id ""]
         (if (<= i 26)
           (str id (char (+ 64 i)))
           (recur (- i 26) (str id (char (+ 64 (mod i 26))))))))))

(defn init-ep-state-tree
  [pdata]
  (zip-ep-state-tree
   (EpistemicState. (make-ep-state-id) [] 0
                    (init-hypspace) [] [] []
                    {:confidence nil :hyps []}
                    [] [] {} pdata)))

(defn current-ep-state
  [ep-state-tree]
  (zip/node ep-state-tree))

(defn goto-ep-state
  [ep-state-tree id]
  (loop [loc (zip-ep-state-tree (zip/root ep-state-tree))]
    (cond (zip/end? loc) nil
          (= id (:id (zip/node loc))) loc
          :else (recur (zip/next loc)))))

(defn update-ep-state-tree
  [ep-state-tree ep-state]
  (zip/replace ep-state-tree ep-state))

(defn ep-state-tree-to-nested
  [root]
  (conj (map ep-state-tree-to-nested (:children root)) (str root)))

(defn print-ep-state-tree
  [ep-state-tree]
  (vijual/draw-tree [(ep-state-tree-to-nested (zip/root ep-state-tree))]))

(defn accept-decision
  [ep-state id]
  (let [accepted (concat (:accepted ep-state) (:hyps (:decision ep-state)))]
    (EpistemicState.
     id
     []
     (inc (:time ep-state))
     (:hypspace ep-state)
     accepted
     []
     (find-unexplained (:hypspace ep-state) accepted)
     {:confidence nil :hyps []}
     [] [] {}
     (:problem-data ep-state))))

(defn measure-decision-confidence
  [ep-state]
  ;; find the minimum confidence of a hypothesis
  (apply min (map (fn [h] (get-confidence (:hypspace ep-state) h))
                  (:hyps (:decision ep-state)))))

(defn find-least-confident-decision
  [ep-state-tree]
  "Finds most recent lowest-confidence decision; returns the epistemic state."
  (loop [e-s-t (zip-ep-state-tree (zip/root ep-state-tree))
         least-conf (zip/node e-s-t)]
    (if (zip/end? e-s-t) least-conf
        (if (<= (:confidence (:decision (zip/node e-s-t)))
                (:confidence (:decision least-conf)))
          (recur (zip/next e-s-t) (zip/node e-s-t))
          (recur (zip/next e-s-t) least-conf)))))

(defn new-child-ep-state
  [ep-state-tree ep-state]
  (let [d (assoc (:decision ep-state)
            :confidence (measure-decision-confidence ep-state))
        ep (assoc ep-state :decision d)
        ep-tree (goto-ep-state (zip/replace ep-state-tree ep) (:id ep))
        ep-child (accept-decision ep (make-ep-state-id ep-tree))
        ep-tree-child (goto-ep-state (zip/append-child ep-tree ep-child) (:id ep-child))]
    ep-tree-child))

(defn new-branch-ep-state
  [ep-state-tree]
  ;; FIXME
  (zip/left (zip/insert-left ep-state-tree
                             (clone-ep-state (zip/node ep-state-tree)
                                             (make-ep-state-id ep-state-tree)
                                             []))))

(defn back-to
  [ep-state time]
  )

(defn add-log-msg
  [ep-state msg]
  (let [entry (LogEntry. msg)]
    (update-in ep-state [:log] conj entry)))

(defn add-abducer-log-msg
  [ep-state hyps msg]
  (let [entry (AbducerLogEntry. (map get-hyp-id-str hyps) msg)]
    (update-in ep-state [:abducer-log] conj entry)))

(defn add-hyp-log-msg
  [ep-state hyp msg]
  (let [entry (HypLogEntry. (get-hyp-id-str hyp) msg)]
    (if (get (:hyp-log ep-state) hyp)
      (update-in ep-state [:hyp-log hyp] conj entry)
      (update-in ep-state [:hyp-log] assoc hyp [entry]))))

(defn add-hyp
  [ep-state hyp explained log-msg]
  (let [hypspace (-> (:hypspace ep-state)
		     (update-in [:hyps] conj hyp)
		     (add-explainers explained [hyp])
		     (set-confidence hyp (get-apriori hyp)))]
    (-> ep-state
	(update-in [:hypothesized] conj hyp)
	(assoc :hypspace hypspace)
	(add-log-msg log-msg)
        (add-abducer-log-msg [hyp] "Adding hypothesis.")
        (add-hyp-log-msg hyp "Adding hypothesis."))))

(defn add-mutual-conflicts
  [ep-state hyps]
  (reduce (fn [ep hyp] (update-in ep [:hypspace] add-conflicts hyp
				  (set/difference hyps #{hyp})))
	  ep-state hyps))

(defn add-mutual-conflicts-all-explainers
  [ep-state hyp]
  (add-mutual-conflicts ep-state (get-explainers (:hypspace ep-state) hyp)))

(defn penalize-conflicts
  [ep-state conflicts log-msg]
  (if (empty? conflicts) ep-state
    (let [hypspace (reduce (fn [hs c] (penalize hs c))
                           (:hypspace ep-state) conflicts)
          ep (-> ep-state
                 (assoc :hypspace hypspace)
                 (add-abducer-log-msg conflicts
                                      (format "Penalizing conflicts: %s."
                                              (get-hyp-ids-str conflicts))))]
      (reduce (fn [ep c] (add-hyp-log-msg ep c log-msg))
              ep conflicts))))

(defn accept-hyp
  [ep-state hyp]
  (let [conflicts (find-conflicts (:hypspace ep-state) #{hyp})]
    (-> ep-state
        (update-in [:decision :hyps] conj hyp)
        (penalize-conflicts conflicts
                            (format "Penalizing due to conflict with accepted hyp %s."
                                    (get-hyp-id-str hyp))))))

(defn force-acceptance
  [ep-state hyp log-msg]
  (-> ep-state
      (accept-hyp hyp)
      (add-abducer-log-msg [hyp]
                           (format "Forcing acceptance of: %s." (get-hyp-id-str hyp)))
      (add-hyp-log-msg hyp "Forcing acceptance.")
      (add-log-msg log-msg)))

(defn accept-explainer-type
  [ep-state explainer hyp type]
  (-> ep-state
      (accept-hyp explainer)
      (add-abducer-log-msg [hyp explainer]
                           (format "Accepting %s %s as explainer of %s."
                                   type
                                   (get-hyp-id-str explainer)
                                   (get-hyp-id-str hyp)))
      (add-hyp-log-msg explainer
                       (format "Accepting as %s explainer of %s."
                               type (get-hyp-id-str hyp)))
      (add-hyp-log-msg hyp
                       (format "Hyp %s accepted as %s explainer."
                               (get-hyp-id-str explainer) type))))

(defn choose-random-hyp
  ([hyps] (rand-nth (vec hyps)))
     
  ([type ep-state hyps]
     (cond (= :smartguess type)
           (let [hs (:hypspace ep-state)
                 threshold (first (sort (map (fn [h] (get-confidence hs h))
                                             hyps)))]
             (rand-nth (vec (filter (fn [h] (= threshold (get-confidence hs h))) hyps))))
           
           :else
           (choose-random-hyp hyps))))

(defn unexplained-helper
  [ep-state]
  (find-unexplained (:hypspace ep-state)
                    (concat (:hyps (:decision ep-state)) (:accepted ep-state))))

(defn guess-type
  [ep-state type]
  (let [unexplained (unexplained-helper ep-state)]
    (if (not-empty unexplained)
     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (let [hyp (choose-random-hyp unexplained)
           explainers (get-explainers (:hypspace ep-state) hyp)
           explainer (if (not-empty explainers)
                       (choose-random-hyp type ep-state explainers))]
       (if explainer
         (accept-explainer-type ep-state explainer hyp (name type))
         ep-state)))))

(defn guess
  [ep-state]
  (guess-type ep-state :guess))

(defn smartguess
  [ep-state]
  (guess-type ep-state :smartguess))

(defn essentials
  [ep-state]
  (let [essentials (find-essentials (:hypspace ep-state)
                                    (unexplained-helper ep-state))]
    (if (not-empty essentials)
      (let [{hyp :hyp explainer :essential} (choose-random-hyp essentials)]
        (accept-explainer-type ep-state explainer hyp "essential")))))

(defn best-threshold
  [threshold type ep-state]
  (let [best (find-best (:hypspace ep-state)
                        (unexplained-helper ep-state)
                        threshold type)]
    (if (not-empty best)
      (let [{hyp :hyp explainer :best chosen-type :type} (choose-random-hyp best)]
        (accept-explainer-type ep-state explainer hyp
                               (if (= type :smartbest)
                                 (format "%s-%d-%s" (name type) threshold chosen-type)
                                 (format "%s-%d" (name type) threshold)))))))

(defn best
  [threshold]
  (partial best-threshold threshold :best))

(defn smartbest
  [threshold]
  (partial best-threshold threshold :smartbest))
