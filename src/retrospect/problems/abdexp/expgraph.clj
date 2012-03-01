(ns retrospect.problems.abdexp.expgraph
  (:use [loom.graph])
  (:use [loom.attr]))

(defn filled?
  [expgraph vertex]
  (= "filled" (attr expgraph vertex :style)))

(defn filled-nodes
  [expgraph]
  (set (filter #(filled? expgraph %) (nodes expgraph))))

(defn fill
  [expgraph vertex]
  (add-attr expgraph vertex :style "filled"))

(defn conflicts?
  [expgraph v1 v2]
  (or (= "dotted" (attr expgraph v1 v2 :style))
      (= "dotted" (attr expgraph v2 v1 :style))))

(defn conflicts-any?
  [expgraph vertex]
  (some #(conflicts? expgraph vertex %) (filled-nodes expgraph)))

(defn set-conflicts
  [expgraph [v1 v2]]
  (-> expgraph (add-edges [v1 v2])
      (add-attr v1 v2 :style "dotted")
      (add-attr v1 v2 :dir "none")))

(defn consistent?
  [expgraph]
  (not-any? #(conflicts-any? expgraph %) (filled-nodes expgraph)))

(defn complete?
  [expgraph]
  (and (consistent? expgraph)
       (every? (fn [v] (some (fn [p] (filled? expgraph p))
                             (incoming expgraph v)))
               (filled-nodes expgraph))))

(defn need-explanation
  [expgraph]
  (filter (fn [v] (not-any? #(filled? expgraph %) (incoming expgraph v)))
          (filled-nodes expgraph)))

(defn cardinality
  [expgraph]
  (count (filled-nodes expgraph)))

