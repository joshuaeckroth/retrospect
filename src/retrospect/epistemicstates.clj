(ns retrospect.epistemicstates
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.confidences])
  (:require [clojure.zip :as zip])
  (:require [clojure.set :as set])
  (:require [vijual :as vijual])
  (:use [loom.graph :only [digraph add-edges add-nodes nodes]])
  (:use [loom.attr :only [add-attr]])
  (:use [loom.io :only [view]])
  (:use [retrospect.state]))

(defprotocol EpistemicStateTree
  (branch? [ep-state] "Is it possible for node to have children?")
  (node-children [ep-state] "Return children of this node.")
  (make-node [ep-state children] "Makes new node from existing node and new children."))

(defrecord EpistemicState
    [id
     children
     time
     workspace
     problem-data
     depgraph]
  Object
  (toString [_] (format "%s %d %s/%s" id time
                        (confidence-str (ws/get-doubt workspace))
                        (if-let [pct (ws/get-unexplained-pct workspace)]
                          (format "%.2f" pct) "?"))))

(defrecord RootNode [children])

(defn clone-ep-state
  [ep-state id children]
  (EpistemicState.
   id
   children
   (:time ep-state)
   (:workspace ep-state)
   (:problem-data ep-state)
   (:depgraph ep-state)))

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
           (let [mult (int (/ i 26))]
             (recur (- i (* mult 26)) (str id (char (+ 65 (dec mult)))))))))))

(defn init-ep-state-tree
  [pdata]
  (zip/down
   (zip-ep-state-tree
    [(EpistemicState. (make-ep-state-id) [] 0
                      (ws/init-workspace)
                      pdata
                      (digraph))])))

(defn root-ep-state?
  [ep-state]
  (= (type ep-state) retrospect.epistemicstates.RootNode))

(defn current-ep-state
  [ep-state-tree]
  (zip/node ep-state-tree))

(defn previous-ep-state
  [ep-state-tree]
  (let [up (zip/up ep-state-tree)]
    (if-not (root-ep-state? (zip/node up)) (zip/node up))))

(defn nth-previous-ep-state
  [ep-state-tree n]
  (loop [i (dec n)
         loc (zip/up ep-state-tree)]
    (if (or (= 1 i) (root-ep-state? (zip/node (zip/up loc))))
      (zip/node loc) (recur (dec i) (zip/up loc)))))

(defn non-accepted-current-ep-state?
  [ep-state-tree]
  (nil? (zip/down ep-state-tree)))

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
  (let [deeper (map ep-state-tree-to-nested-helper (:children ep-state))]
    (conj deeper (str ep-state))))

(defn ep-state-tree-to-nested
  [ep-state-tree]
  (conj (map ep-state-tree-to-nested-helper (:children (zip/root ep-state-tree)))
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
  "List ep-states in the order that they were created (i.e., sorted by id,
   which is the same as a depth-first left-first walk)."
  [ep-state-tree]
  (map str (flatten-ep-state-tree ep-state-tree)))

(defn add-hyp-helper
  [ep-state hyp explains dep-node depends & opts]
  (let [g (if (empty? depends) (:depgraph ep-state)
            (reduce (fn [g d] (-> g (add-edges [dep-node d]) (add-attr d :label (:str d))))
                    (-> (:depgraph ep-state)
                        (add-nodes dep-node)
                        (add-attr dep-node :label (:str dep-node)))
                    depends))]
    (assoc ep-state
      :workspace (apply ws/add (:workspace ep-state) hyp explains opts)
      :depgraph g)))

(defn add-hyp
  [ep-state hyp explains dep-node depends]
  (add-hyp-helper ep-state hyp explains dep-node depends :static))

(defn add-more-hyp
  [ep-state hyp explains dep-node depends]
  (add-hyp-helper ep-state hyp explains dep-node depends))

(defn add-fact
  [ep-state hyp explains]
  (update-in ep-state [:workspace]
             #(-> % (ws/add hyp explains :static) (ws/forced hyp))))

(defn commit-decision
  [ep-state id time-now]
  (let [workspace (:workspace ep-state)
        accepted (:accepted workspace)
        rejected (:rejected workspace)]
    (EpistemicState.
     id
     []
     (inc time-now)
     (ws/init-workspace)
     ((:commit-decision-fn @problem) (:problem-data ep-state)
      accepted rejected time-now)
     (:depgraph ep-state))))

(defn new-branch-ep-state
  [ep-state-tree branch]
  (let [ep-state (current-ep-state ep-state-tree)
        ep-tree (goto-ep-state (zip/replace ep-state-tree ep-state) (:id ep-state))
        ep (clone-ep-state branch (make-ep-state-id ep-tree) [])

        ;; make a branch; the choice of "insert-right" over "insert-left" here
        ;; is what makes (list-ep-states) possible, since depth-first search
        ;; looks left before looking right
        ep-tree-branch
        (goto-ep-state (zip/insert-right (goto-ep-state ep-tree (:id branch)) ep) (:id ep))]
    ep-tree-branch))

(defn new-branch-root
  [ep-state-tree pdata]
  (let [ep-state (current-ep-state ep-state-tree)
        est (goto-ep-state (zip/replace ep-state-tree ep-state) "A")
        new-ep (EpistemicState. (make-ep-state-id est) [] 0
                                (ws/init-workspace) pdata (digraph))]
    (goto-ep-state (zip/insert-right (goto-ep-state est "A") new-ep) (:id new-ep))))

(defn new-child-ep-state
  [ep-state-tree ep-state time-now]
  (let [ep-tree (goto-ep-state (zip/replace ep-state-tree ep-state) (:id ep-state))
        ep-child (commit-decision ep-state (make-ep-state-id ep-tree) time-now)
        ep-tree-child (goto-ep-state (zip/append-child ep-tree ep-child) (:id ep-child))]
    ep-tree-child))

(defn explain
  [ep-state & opts]
  (let [workspace (if (some #{:no-prepare} opts) (:workspace ep-state)
                      (ws/prepare-workspace (:workspace ep-state)))
        pdata (:problem-data ep-state)
        ws-explained (ws/explain workspace pdata)]
    (if (not-empty (:unexplained (:final (:log ws-explained))))
      (if-let [ep-more-hyps ((:get-more-hyps-fn @problem) (assoc ep-state :workspace ws-explained))]
        (assoc ep-more-hyps :workspace (ws/explain (:workspace ep-more-hyps)
                                                   (:problem-data ep-more-hyps)))
        (assoc ep-state :workspace ws-explained))
      (assoc ep-state :workspace ws-explained))))
