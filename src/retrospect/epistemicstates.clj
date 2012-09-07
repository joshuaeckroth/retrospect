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
    [id children cycle time decision-point results workspace meta-est]
  Object
  (toString [_]
    (format "%s %d/%d %s/%.2f%s" id cycle time
       (if-let [d ((:calc-doubt-fn @reasoner) workspace)] (format "%.2f" d) "?")
       ((:calc-coverage-fn @reasoner) workspace)
       (if decision-point " *" ""))))

(defrecord RootNode [children workspace meta-est])

(defn clone-ep
  [ep id children]
  (EpistemicState. id children (:cycle ep) (:time ep) (:decision-point ep)
                   (:results ep) (:workspace ep) nil))

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
  [ep-states workspace meta-est]
  (zip/zipper branch? node-children make-node
              (RootNode. ep-states workspace meta-est)))

(defn flatten-est
  [est]
  (if (nil? est) []
      (loop [loc (zip/down (zip-est (:children (zip/root est))
                                    (:workspace (zip/root est))
                                    (:meta-est (zip/root est))))
             states []]
        (if (zip/end? loc) states
            (recur (zip/next loc) (conj states (zip/node loc)))))))

(defn make-ep-id
  ([] "0001")
  ([est] (format "%04d" (count (flatten-est est)))))

(defn init-est
  [workspace]
  (zip/down (zip-est [(EpistemicState. (make-ep-id) [] 0 0 false [] workspace nil)]
                     workspace nil)))

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

(defn goto-ep
  [est id]
  (loop [loc (zip/down (zip-est (:children (zip/root est))
                                (:workspace (zip/root est))
                                (:meta-est (zip/root est))))]
    (cond (zip/end? loc) nil
          (= id (:id (zip/node loc))) loc
          :else (recur (zip/next loc)))))

(defn goto-cycle
  [est cycle]
  (loop [loc est]
    (if (or (= cycle (:cycle (zip/node loc)))
            (root-ep? (zip/node (zip/up loc))))
      loc
      (recur (zip/up loc)))))

(defn goto-start-of-time
  [est time]
  (let [t (max 0 time)]
    (loop [loc est]
      (if (or (root-ep? (zip/node (zip/up loc)))
              (= (dec time) (:time (zip/node (zip/up loc)))))
        loc
        (recur (zip/up loc))))))

(defn count-branches
  [est]
  (+ (count (filter #(second (:children %))
               (flatten-est est)))
     (dec (count (:children (zip/root est))))))

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

(defn ep-path
  [est]
  (loop [loc (zip/up est) ;; skip leaf ep which is just a clone of the previous
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
  (let [ep (assoc (clone-ep branch (make-ep-id est) [])
             :decision-point false)]
    ;; make a branch; the choice of "insert-right" over "insert-left" here
    ;; is what makes (list-ep-states) possible, since depth-first search
    ;; looks left before looking right
    (zip/right (zip/insert-right (goto-ep est (:id branch)) ep))))

(defn new-child-ep
  [est]
  (let [ep-child (assoc (clone-ep (cur-ep est) (make-ep-id est) [])
                   :decision-point false)
        cycle-child (inc (:cycle (cur-ep est)))]
    (zip/down (zip/append-child est (assoc ep-child :cycle cycle-child)))))
