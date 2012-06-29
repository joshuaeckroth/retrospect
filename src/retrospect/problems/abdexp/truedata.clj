(ns retrospect.problems.abdexp.truedata
  (:use [clojure.set :only [difference]])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr])
  (:use [clojure.contrib.combinatorics])
  (:require [clojure.set :as set])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn arbitrary-path-up
  [expgraph v]
  (loop [path [v]]
    (let [ns (incoming expgraph (last path))]
      (if (empty? ns) path
          (recur (conj path (my-rand-nth (sort ns))))))))

(defn random-expgraph-levels
  []
  (let [vertices (my-shuffle (range (:NumVertices params)))
        vs-levels (loop [vs-levels []
                         vs vertices
                         cuts (repeatedly (:NumLevels params)
                                          #(inc (my-rand-int (/ (:NumVertices params)
                                                                (:NumLevels params)))))]
                    (if (empty? cuts) vs-levels
                        (recur (conj vs-levels (take (first cuts) vs))
                               (drop (first cuts) vs)
                               (rest cuts))))
        expl-links (set (mapcat (fn [[vs1 vs2]]
                                  (mapcat (fn [v2]
                                            (map (fn [v1] [v1 v2])
                                               (take (inc (my-rand-int (:MaxExplainLinks params)))
                                                     (my-shuffle vs1))))
                                          vs2))
                                (partition 2 1 vs-levels)))
        eg (connect (apply add-edges (digraph) expl-links))
        eg-filled (reduce fill eg (last vs-levels))
        true-vertices (set/difference (set (apply concat (for [v (last vs-levels)]
                                                           (arbitrary-path-up eg-filled v))))
                                      (set (last vs-levels)))
        non-data (set (apply concat (butlast vs-levels)))
        conflict-links (let [vs (sort (set/difference
                                       (nodes eg-filled) (set (last vs-levels)) true-vertices))]
                         (take (:MaxConflictLinks params)
                               (my-shuffle
                                (sort #(let [c (compare (first %1) (first %2))]
                                         (if (= 0 c) (compare (second %1) (second %2)) c))
                                      (set (filter (fn [[v1 v2]]
                                                (and (not= v1 v2)
                                                     (not (has-edge? eg v1 v2))
                                                     (not (has-edge? eg v2 v1))))
                                              (combinations vs 2)))))))
        eg-conflicts (reduce set-conflicts eg-filled conflict-links)
        eg-scores (reduce (fn [eg v] (add-attr eg v :score
                                         (if (non-data v)
                                           (if (true-vertices v)
                                             (min 1.0 (+ (my-rand) (my-rand)))
                                             (max 0.0 (- (my-rand) (my-rand))))
                                           1.0)))
                     eg-conflicts (sort (nodes eg-conflicts)))
        eg-forced (apply force-fill eg-scores (bottom-nodes eg-scores))]
    (if (empty? (forced-nodes eg-forced)) (random-expgraph-levels)
        {:expgraph eg-forced :true-vertices true-vertices})))

(defn generate-truedata
  []
  (let [expgraphs-true-vertices (repeatedly (:Steps params) #(random-expgraph-levels))
        expgraphs (zipmap (range 1 (inc (:Steps params)))
                          (map :expgraph expgraphs-true-vertices))]
    {:training {:test expgraphs
                :true-vertices (zipmap (range 1 (inc (:Steps params)))
                                       (map :true-vertices expgraphs-true-vertices))}
     :test expgraphs
     :true-vertices (zipmap (range 1 (inc (:Steps params)))
                            (map :true-vertices expgraphs-true-vertices))}))
