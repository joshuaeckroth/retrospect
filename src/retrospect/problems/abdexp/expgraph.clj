(ns retrospect.problems.abdexp.expgraph
  (:use [clojure.set])
  (:use [loom.io])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr]))

(defn vertex?
  [expgraph vertex]
  ((nodes expgraph) vertex))

(defn score
  [expgraph vertex value]
  (or (get (attr expgraph vertex :scores) value 0.0) 0.0))

(defn scores
  [expgraph vertex]
  (or (attr expgraph vertex :scores) []))

(defn values
  [expgraph vertex]
  (or (attr expgraph vertex :values) []))

(defn value
  [expgraph vertex]
  (or (attr expgraph vertex :value) "off"))

(defn probs
  [expgraph vertex]
  (or (attr expgraph vertex :probs) []))

(defn observation?
  [vertex]
  (= "O" (subs vertex 0 1)))

(defn forced?
  [expgraph vertex]
  (attr expgraph vertex :forced))

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
       (not-any? (fn [v] (= "on" (value expgraph v)))
                 (explainers expgraph vertex))))

(defn forced-nodes
  [expgraph]
  (set (filter #(forced? expgraph %) (nodes expgraph))))

(defn on-nodes
  [expgraph]
  (set (filter #(= "on" (value expgraph %)) (nodes expgraph))))

(defn unexplained-nodes
  [expgraph]
  (set (filter #(unexplained? expgraph %) (on-nodes expgraph))))

(defn accepted-nodes
  [expgraph]
  (difference (on-nodes expgraph) (forced-nodes expgraph)))

(defn bottom-nodes
  [expgraph]
  (set (filter #(empty? (neighbors expgraph %)) (nodes expgraph))))

(defn top-nodes
  [expgraph]
  (set (filter #(empty? (incoming expgraph %)) (nodes expgraph))))

(defn vertices
  [expgraph]
  (nodes expgraph))

(defn data-explained-by-top
  [expgraph]
  (let [eg-filled (apply remove-nodes expgraph
                         (difference (nodes expgraph)
                                     (on-nodes expgraph)))
        explained-vs (set (mapcat #(pre-traverse eg-filled %)
                                  ;; be sure to refer back to original expgraph
                                  (top-nodes expgraph)))]
    (intersection explained-vs (forced-nodes expgraph))))

(defn turn-on
  [expgraph & vertices]
  (reduce (fn [g v] (add-attr g v :value "on")) expgraph vertices))

(defn force-on
  [expgraph & vertices]
  (reduce (fn [g v] (-> g (turn-on v) (add-attr v :forced true))) expgraph vertices))

(defn conflicts?
  [expgraph v1 v2]
  (or (= "dotted" (attr expgraph v1 v2 :style))
      (= "dotted" (attr expgraph v2 v1 :style))))

(defn conflicts-any?
  [expgraph vertex]
  (some #(conflicts? expgraph vertex %) (on-nodes expgraph)))

(defn set-conflicts
  [expgraph & pairs]
  (reduce (fn [g [v1 v2]]
       (-> g (add-edges [v1 v2])
          (add-attr v1 v2 :style "dotted")
          (add-attr v1 v2 :dir "none")
          (add-attr v1 v2 :constraint false)))
     expgraph pairs))

(defn consistent?
  [expgraph]
  (not-any? #(conflicts-any? expgraph %) (on-nodes expgraph)))

(defn complete?
  [expgraph]
  (and (consistent? expgraph)
       (every? (fn [v] (some (fn [p] (= "on" (value expgraph p)))
                         (explainers expgraph v)))
               (on-nodes expgraph))))

(defn need-explanation
  [expgraph]
  (filter (fn [v] (and (not-empty (explainers expgraph v))
                 (not-any? #(= "on" (value expgraph %))
                           (explainers expgraph v))))
     (on-nodes expgraph)))

(defn sorted-by-dep
  ([expgraph]
     (sorted-by-dep expgraph (bottom-nodes expgraph)))
  ([expgraph starts]
     (rest (topsort (reduce (fn [g v] (add-edges g [-1 v]))
                       (let [conflicts-edges (filter #(apply conflicts? expgraph %)
                                                (edges expgraph))]
                         (transpose (apply remove-edges expgraph conflicts-edges)))
                       starts)
                    -1))))

(defn format-dot-expgraph
  [expgraph]
  (reduce (fn [eg v]
       (add-attr eg v :label (format "%s / %.2f" (str v) (or (attr eg v :score) 1.0))))
     expgraph (nodes expgraph)))
