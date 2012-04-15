(ns retrospect.epistemicstates
  (:require [clojure.zip :as zip])
  (:require [clojure.set :as set])
  (:require [vijual :as vijual])
  (:use [loom.graph :only [digraph add-edges add-nodes remove-nodes
                           nodes edges neighbors has-edge?]])
  (:use [loom.alg :only [pre-traverse]])
  (:use [loom.attr :only [add-attr]])
  (:use [loom.io :only [view]])
  (:use [clojure.java [io :only [file]] [shell :only [sh]]])
  (:use [retrospect.state]))

(defprotocol EpistemicStateTree
  (branch? [ep] "Is it possible for node to have children?")
  (node-children [ep] "Return children of this node.")
  (make-node [ep children] "Makes new node from existing node and new children."))

(defrecord EpistemicState
    [id children time workspace]
  Object
  (toString [_]
    (format "%s %d %.2f/%.2f" id time (:doubt workspace) (:coverage workspace))))

(defrecord RootNode [children workspace])

(defn clone-ep
  [ep id children]
  (EpistemicState. id children (:time ep) (:workspace ep)))

(extend-protocol EpistemicStateTree
  EpistemicState
  (branch? [ep] true)
  (node-children [ep] (seq (:children ep)))
  (make-node [ep children] (clone-ep ep (:id ep) children))
  RootNode
  (branch? [root] true)
  (node-children [root] (seq (:children root)))
  (make-node [ep children] (assoc ep :children children)))

(defn zip-est
  [ep-states workspace]
  (zip/zipper branch? node-children make-node (RootNode. ep-states workspace)))

(defn make-ep-id
  ([]
     "A")
  ([ep-state-tree]
     (let [count
           (loop [count 0
                  loc (zip/down (zip-est (:children (zip/root ep-state-tree))
                                         (:workspace (zip/root ep-state-tree))))]
             (if (zip/end? loc) count
                 (recur (inc count) (zip/next loc))))]
       (loop [i count
              id ""]
         (if (<= i 25)
           (str id (char (+ 65 i)))
           (let [mult (int (/ i 26))]
             (recur (- i (* mult 26)) (str id (char (+ 65 (dec mult)))))))))))

(defn init-est
  [workspace]
  (zip/down (zip-est [(EpistemicState. (make-ep-id) [] 0 workspace)] workspace)))

(defn get-init-workspace
  [est]
  (:workspace (zip/root est)))

(defn root-ep?
  [ep-state]
  (= (type ep-state) retrospect.epistemicstates.RootNode))

(defn cur-ep
  [est]
  (zip/node est))

(defn prev-ep
  [est]
  (let [up (zip/up est)]
    (when-not (root-ep? (zip/node up)) (zip/node up))))

(defn ep-state-depth
  [est]
  (loop [i 0
         loc est]
    (if (root-ep? (zip/node loc)) i
        (recur (inc i) (zip/up loc)))))

(defn nth-previous-ep
  [est n]
  (loop [i (or n (ep-state-depth est))
         loc est]
    (if (or (= 1 i) (root-ep? (zip/node (zip/up loc))))
      (zip/node loc) (recur (dec i) (zip/up loc)))))

(defn goto-ep
  [est id]
  (loop [loc (zip/down (zip-est (:children (zip/root est))
                                (:workspace (zip/root est))))]
    (cond (zip/end? loc) nil
          (= id (:id (zip/node loc))) loc
          :else (recur (zip/next loc)))))

(defn update-est
  [est ep]
  (zip/replace est ep))

(defn est-to-nested-helper
  [ep]
  (let [deeper (map est-to-nested-helper (:children ep))]
    (conj deeper (str ep))))

(defn est-to-nested
  [est]
  (conj (map est-to-nested-helper (:children (zip/root est)))
        "root"))

(defn print-est
  [est]
  (vijual/draw-tree [(est-to-nested est)]))

(defn draw-est
  [est]
  (vijual/draw-tree-image [(est-to-nested est)]))

(defn flatten-est
  [est]
  (if (nil? est) []
      (loop [loc (zip/down (zip-est (:children (zip/root est))
                                    (:workspace (zip/root est))))
             states []]
        (if (zip/end? loc) states
            (recur (zip/next loc) (conj states (zip/node loc)))))))

(defn ep-path
  [est]
  (loop [loc est
         states []]
    (if (root-ep? (zip/node loc)) (reverse states)
        (recur (zip/up loc) (conj states (zip/node loc))))))

(defn list-ep-states
  "List ep-states in the order that they were created (i.e., sorted by id,
   which is the same as a depth-first left-first walk)."
  [est]
  (map str (flatten-est est)))

(defn new-branch-ep
  [est branch]
  (let [ep (clone-ep branch (make-ep-id est) [])]
    ;; make a branch; the choice of "insert-right" over "insert-left" here
    ;; is what makes (list-ep-states) possible, since depth-first search
    ;; looks left before looking right
    (zip/right (zip/insert-right (goto-ep est (:id branch)) ep))))

(defn new-child-ep
  [est time-now]
  (let [ep-child (clone-ep (cur-ep est) (make-ep-id est) [])]
    (zip/down (zip/append-child est (assoc ep-child :time time-now)))))
