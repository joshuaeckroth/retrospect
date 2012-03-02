(ns retrospect.problems.abdexp.expgraph
  (:use [loom.io])
  (:use [loom.graph])
  (:use [loom.attr]))

(defn score
  [expgraph vertex]
  (attr expgraph vertex :score))

(defn filled?
  [expgraph vertex]
  (= "filled" (attr expgraph vertex :style)))

(defn explainers
  [expgraph vertex]
  (filter #(and (not= "none" (attr expgraph vertex % :dir))
                (not= "none" (attr expgraph % vertex :dir)))
          (incoming expgraph vertex)))

(defn unexplained?
  [expgraph vertex]
  (not-any? (fn [v] (filled? expgraph v)) (explainers expgraph vertex)))

(defn filled-nodes
  [expgraph]
  (set (filter #(filled? expgraph %) (nodes expgraph))))

(defn unexplained-nodes
  [expgraph]
  (set (filter #(unexplained? expgraph %) (filled-nodes expgraph))))

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
                             (explainers expgraph v)))
               (filled-nodes expgraph))))

(defn need-explanation
  [expgraph]
  (filter (fn [v] (not-any? #(filled? expgraph %) (explainers expgraph v)))
          (filled-nodes expgraph)))

(defn composite-score
  [expgraph]
  (reduce + (map (fn [v] (score expgraph v)) (filled-nodes expgraph))))

(defn format-dot-expgraph
  [expgraph]
  (reduce (fn [eg v]
            (add-attr eg v :label (format "%d / %.2f" v (attr eg v :score))))
          expgraph (nodes expgraph)))
