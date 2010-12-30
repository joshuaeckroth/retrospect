(ns simulator.epistemicstates
  (:require [simulator logs])
  (:import [simulator.logs LogEntry])
  (:require [simulator.workspaces :as ws :only
             [init-workspace accept-workspace-decision get-decision-confidence
              update-decision-confidence log-final-accepted-rejeted-hyps add-hyp
              force-acceptance reset-confidences-to-apriori lookup-hyps]])
  (:use [simulator.confidences])
  (:require [clojure.zip :as zip])
  (:require [clojure.set :as set])
  (:require [vijual :as vijual]))

(defprotocol EpistemicStateTree
  (branch? [ep-state] "Is it possible for node to have children?")
  (node-children [ep-state] "Return children of this node.")
  (make-node [ep-state children] "Makes new node from existing node and new children."))

(defrecord EpistemicState
    [id
     children
     time
     workspace
     log
     problem-data]
  Object
  (toString [_] (format "%s %d %s" id time
                        (confidence-str (:confidence (:decision workspace))))))

(defn clone-ep-state
  [ep-state id children]
  (EpistemicState.
   id
   children
   (:time ep-state)
   (:workspace ep-state)
   (:log ep-state)
   (:problem-data ep-state)))

(defrecord RootNode [children])

(extend-protocol EpistemicStateTree
  EpistemicState
  (branch? [ep-state] true)
  (node-children [ep-state] (seq (:children ep-state)))
  (make-node [ep-state children] (clone-ep-state ep-state (:id ep-state) children))
  RootNode
  (branch? [root] true)
  (node-children [root] (seq (:children root)))
  (make-node [ep-state children] (assoc ep-state :children children)))

(defn zip-ep-state-tree
  [ep-states]
  (zip/zipper branch? node-children make-node (RootNode. ep-states)))

(defn make-ep-state-id
  ([]
     "A")
  ([ep-state-tree]
     (let [count
           (loop [count 0
                  loc (zip/down (zip-ep-state-tree (:children (zip/root ep-state-tree))))]
             (if (zip/end? loc) count
                 (recur (inc count) (zip/next loc))))]
       (loop [i count
              id ""]
         (if (<= i 25)
           (str id (char (+ 65 i)))
           (recur (- i 26) (str id (char (+ 65 (mod i 26))))))))))

(defn init-ep-state-tree
  [pdata]
  (zip/down
   (zip-ep-state-tree
    [(EpistemicState. (make-ep-state-id) [] 0
                      (ws/init-workspace)
                      []
                      pdata)])))

(defn root-ep-state?
  [ep-state]
  (= (type ep-state) simulator.epistemicstates.RootNode))

(defn current-ep-state
  [ep-state-tree]
  (zip/node ep-state-tree))

(defn previous-ep-state
  [ep-state-tree]
  (let [up (zip/up ep-state-tree)]
    (if-not (root-ep-state? (zip/node up)) (zip/node up))))

(defn goto-next-ep-state
  [ep-state-tree]
  (zip/next ep-state-tree))

(defn left-ep-state
  [ep-state-tree]
  (zip/node (zip/left ep-state-tree)))

(defn goto-ep-state
  [ep-state-tree id]
  (loop [loc (zip/down (zip-ep-state-tree (:children (zip/root ep-state-tree))))]
    (cond (zip/end? loc) nil
          (= id (:id (zip/node loc))) loc
          :else (recur (zip/next loc)))))

(defn update-ep-state-tree
  [ep-state-tree ep-state]
  (zip/replace ep-state-tree ep-state))

(defn ep-state-tree-to-nested-helper
  [ep-state]
  (let [deeper (map ep-state-tree-to-nested-helper
                    (filter (comp :confidence :decision :workspace)
                            (:children ep-state)))]
    (conj deeper (str ep-state))))

(defn ep-state-tree-to-nested
  [ep-state-tree]
  (conj (map ep-state-tree-to-nested-helper
             (filter (comp :confidence :decision :workspace)
                     (:children (zip/root ep-state-tree))))
        "root"))

(defn print-ep-state-tree
  [ep-state-tree]
  (vijual/draw-tree [(ep-state-tree-to-nested ep-state-tree)]))

(defn draw-ep-state-tree
  [ep-state-tree]
  (vijual/draw-tree-image [(ep-state-tree-to-nested ep-state-tree)]))

(defn flatten-ep-state-tree
  [ep-state-tree]
  (loop [loc (goto-ep-state ep-state-tree "A")
         states []]
    (if (zip/end? loc) states
        (recur (zip/next loc) (conj states (zip/node loc))))))

(defn list-ep-states
  [ep-state-tree]
  "List ep-states in the order that they were created (i.e., sorted by id,
   which is the same as a depth-first left-first walk)."
  (map str (filter (comp :confidence :decision :workspace)
                   (flatten-ep-state-tree ep-state-tree))))

(defn add-log-msg
  [ep-state msg]
  (let [entry (LogEntry. msg)]
    (update-in ep-state [:log] conj entry)))

(defn add-hyp
  [ep-state hyp]
  (update-in ep-state [:workspace] ws/add-hyp hyp))

(defn force-acceptance
  [ep-state hyp]
  (update-in ep-state [:workspace] ws/force-acceptance hyp))

(defn accept-decision
  [ep-state id]
  (let [workspace (:workspace ep-state)
        accepted-hyps (ws/lookup-hyps workspace (:accepted (:decision workspace)))]
    (EpistemicState.
     id
     []
     (inc (:time ep-state))
     (ws/accept-workspace-decision workspace)
     []
     (doall (reduce (fn [pdata action] (action pdata))
                    (:problem-data ep-state)
                    (map :update-fn accepted-hyps)) ))))

(defn find-least-confident-decision
  [ep-state-tree]
  "Finds most recent (up the path) lowest-confidence decision; returns
   the epistemic state; returns nil if there are no past states."
  (if-not (root-ep-state? (zip/node (zip/up ep-state-tree)))
    (loop [loc (zip/up ep-state-tree)
           least-conf (zip/node loc)]
      (cond
       (root-ep-state? (zip/node loc)) least-conf

       (< (ws/get-decision-confidence (:workspace (zip/node loc)))
          (ws/get-decision-confidence (:workspace least-conf)))
       (recur (zip/up loc) (zip/node loc))
       
       :else
       (recur (zip/up loc) least-conf)))))

(defn update-decision
  [ep-state-tree ep-state]
  (let [ep (update-in ep-state [:workspace] ws/update-decision-confidence)
        ep-tree (goto-ep-state (zip/replace ep-state-tree ep) (:id ep))]
    ep-tree))

(defn count-branches
  [ep-state-tree branch]
  (count (zip/children (zip/up (goto-ep-state ep-state-tree (:id branch))))))

(defn new-branch-ep-state
  [ep-state-tree branch]
  (let [ep-tree (update-decision ep-state-tree (current-ep-state ep-state-tree))
        ep (clone-ep-state branch (make-ep-state-id ep-tree) [])

        ;; make a branch; the choice of "insert-right" over "insert-left" here
        ;; is what makes (list-ep-states) possible, since depth-first search
        ;; looks left before looking right
        ep-tree-branch
        (goto-ep-state (zip/insert-right (goto-ep-state ep-tree (:id branch)) ep)
                       (:id ep))]
    ep-tree-branch))

(defn new-child-ep-state
  [ep-state-tree ep-state]
  (let [ep-with-log (update-in ep-state [:workspace] ws/log-final-accepted-rejected-hyps)
        ep-tree (update-decision ep-state-tree ep-with-log)
        ep-child (accept-decision ep-with-log (make-ep-state-id ep-tree))
        ep-child-fresh (update-in ep-child [:workspace]
                                  ws/delete-ancient-hyps (:time ep-child))
        ep-tree-child (goto-ep-state (zip/append-child ep-tree ep-child-fresh)
                                     (:id ep-child-fresh))]
    ep-tree-child))

(defn generate-hyps-and-explain
  [problem ep-state sensors params lazy]
  (let [ep-state-with-hyps ((:get-more-hyps-fn problem) ep-state sensors params lazy)
        ws-explained (ws/explain (:workspace ep-state-with-hyps))]
    (assoc ep-state-with-hyps :workspace ws-explained)))
