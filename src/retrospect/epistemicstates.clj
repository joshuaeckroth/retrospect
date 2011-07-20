(ns retrospect.epistemicstates
  (:require [retrospect.workspaces :as ws])
  (:use [retrospect.confidences])
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
     problem-data]
  Object
  (toString [_] (format "%s %d %s" id time (confidence-str (ws/get-conf workspace)))))

(defrecord RootNode [children])

(defn clone-ep-state
  [ep-state id children]
  (EpistemicState.
   id
   children
   (:time ep-state)
   (:workspace ep-state)
   (:problem-data ep-state)))

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
                      pdata)])))

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
  (let [deeper (map ep-state-tree-to-nested-helper
                    (filter (comp ws/get-conf :workspace) (:children ep-state)))]
    (conj deeper (str ep-state))))

(defn ep-state-tree-to-nested
  [ep-state-tree]
  (conj (map ep-state-tree-to-nested-helper
             (filter (comp ws/get-conf :workspace) (:children (zip/root ep-state-tree))))
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
  (map str (filter (comp ws/get-conf :workspace)
                   (flatten-ep-state-tree ep-state-tree))))

(defn add-hyp
  [ep-state hyp explains]
  (update-in ep-state [:workspace] ws/add hyp explains :static))

(defn add-more-hyp
  [ep-state hyp explains]
  (update-in ep-state [:workspace] ws/add hyp explains))

(defn add-fact
  [ep-state hyp explains]
  (update-in ep-state [:workspace]
             #(-> % (ws/add hyp explains :static) (ws/forced hyp))))

(defn commit-decision
  [ep-state id time-now problem]
  (let [workspace (:workspace ep-state)
        accepted (:accepted workspace)
        rejected (:rejected workspace)]
    (EpistemicState.
     id
     []
     (inc time-now)
     (ws/init-workspace workspace)
     ((:commit-decision-fn problem) (:problem-data ep-state) accepted))))

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
  [ep-state-tree]
  (let [ep-state (current-ep-state ep-state-tree)
        est (goto-ep-state (zip/replace ep-state-tree ep-state) "A")
        new-ep (clone-ep-state (current-ep-state est)
                               (make-ep-state-id est) [])
        new-ep-ws (assoc new-ep :workspace (ws/init-workspace))]
    (goto-ep-state (zip/insert-right (goto-ep-state est "A") new-ep-ws)
                   (:id new-ep-ws))))

(defn new-child-ep-state
  [ep-state-tree ep-state time-now problem]
  (let [ep-tree (goto-ep-state (zip/replace ep-state-tree ep-state) (:id ep-state))
        ep-child (commit-decision ep-state (make-ep-state-id ep-tree) time-now problem)
        ep-tree-child (goto-ep-state (zip/append-child ep-tree ep-child) (:id ep-child))]
    ep-tree-child))

(defn explain
  [ep-state get-more-hyps inconsistent & opts]
  (let [workspace (if (some #{:no-prepare} opts) (:workspace ep-state)
                      (ws/prepare-workspace (:workspace ep-state)))
        pdata (:problem-data ep-state)
        ws-explained (ws/explain workspace inconsistent pdata)]
    (if (not-empty (:unexplained (:final (:log ws-explained))))
      (let [ep-explained (assoc ep-state :workspace ws-explained)
            ep-more-hyps (get-more-hyps ep-explained)
            ws-more-explained (ws/explain (:workspace ep-more-hyps)
                                          inconsistent
                                          (:problem-data ep-more-hyps))]
        (assoc ep-more-hyps :workspace ws-more-explained))
      (assoc ep-state :workspace ws-explained))))
