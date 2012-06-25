(ns retrospect.problems.abdexp.expgraph
  (:use [clojure.set])
  (:use [loom.io])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr]))

(defn score
  [expgraph vertex]
  (attr expgraph vertex :score))

(defn filled?
  [expgraph vertex]
  (= "filled" (attr expgraph vertex :style)))

(defn explains
  [expgraph vertex]
  (filter #(and (not= "none" (attr expgraph vertex % :dir))
           (not= "none" (attr expgraph % vertex :dir)))
     (neighbors expgraph vertex)))

(defn explainers
  [expgraph vertex]
  (filter #(and (not= "none" (attr expgraph vertex % :dir))
           (not= "none" (attr expgraph % vertex :dir)))
     (incoming expgraph vertex)))

(defn unexplained?
  [expgraph vertex]
  (and (not-empty (explainers expgraph vertex))
       (not-any? (fn [v] (filled? expgraph v)) (explainers expgraph vertex))))

(defn filled-nodes
  [expgraph]
  (set (filter #(filled? expgraph %) (nodes expgraph))))

(defn unexplained-nodes
  [expgraph]
  (set (filter #(unexplained? expgraph %) (filled-nodes expgraph))))

(defn data-nodes
  [expgraph]
  (set (filter #(empty? (neighbors expgraph %)) (nodes expgraph))))

(defn accepted-nodes
  [expgraph]
  (difference (filled-nodes expgraph) (data-nodes expgraph)))

(defn top-nodes
  [expgraph]
  (set (filter #(empty? (incoming expgraph %)) (nodes expgraph))))

(defn data-explained-by-top
  [expgraph]
  (let [eg-filled (apply remove-nodes expgraph
                         (difference (nodes expgraph)
                                     (filled-nodes expgraph)))
        explained-vs (set (mapcat #(pre-traverse eg-filled %)
                                  ;; be sure to refer back to original expgraph
                                  (top-nodes expgraph)))]
    (intersection explained-vs (data-nodes expgraph))))

(defn fill
  [expgraph & vertices]
  (reduce (fn [g v] (add-attr g v :style "filled")) expgraph vertices))

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
     (add-attr v1 v2 :dir "none")
     (add-attr v1 v2 :constraint false)))

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
  (filter (fn [v] (and (not-empty (explainers expgraph v))
                 (not-any? #(filled? expgraph %) (explainers expgraph v))))
     (filled-nodes expgraph)))

(defn format-dot-expgraph
  [expgraph]
  (reduce (fn [eg v]
       (add-attr eg v :label (format "%s / %.2f" (str v) (or (attr eg v :score) 1.0))))
     expgraph (nodes expgraph)))
