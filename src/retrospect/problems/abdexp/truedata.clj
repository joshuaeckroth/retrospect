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
  (let [obs-possible (set (repeatedly (* 10 (:Steps params))
                                      #(format "O%d" (my-rand-int 1000))))
        vertices (set (repeatedly (:NumVertices params)
                                  #(format "E%d" (+ 1000 (my-rand-int 1000)))))
        vs-levels (loop [vs-levels []
                         vs vertices
                         cuts (repeatedly (:NumLevels params)
                                          #(inc (my-rand-int (/ (:NumVertices params)
                                                                (:NumLevels params)))))]
                    (if (empty? cuts) (conj (vec vs-levels) obs-possible)
                        (recur (conj vs-levels (take (first cuts) vs))
                               (drop (first cuts) vs)
                               (rest cuts))))
        expl-links (set (mapcat
                         (fn [[vs1 vs2]]
                           (mapcat (fn [v1]
                                     (map (fn [v2] [v1 v2])
                                        (take (inc (my-rand-int (:MaxExplainLinks params)))
                                              (my-shuffle vs2))))
                                   vs1))
                         (partition 2 1 vs-levels)))
        eg (apply add-edges (digraph) expl-links)
        observations (set (filter #(not-empty (incoming eg %)) (last vs-levels)))
        true-vertices (set (apply concat (for [v observations]
                                           (arbitrary-path-up eg v))))
        non-data (set (apply concat (butlast vs-levels)))
        conflict-links (let [vs (sort (set/difference
                                       (nodes eg)
                                       observations
                                       true-vertices))]
                         (take (:MaxConflictLinks params)
                               (my-shuffle
                                (sort #(let [c (compare (first %1) (first %2))]
                                         (if (= 0 c) (compare (second %1) (second %2)) c))
                                      (set (filter (fn [[v1 v2]]
                                                (and (not= v1 v2)
                                                     (not (has-edge? eg v1 v2))
                                                     (not (has-edge? eg v2 v1))))
                                              (combinations vs 2)))))))
        eg-conflicts (reduce set-conflicts eg conflict-links)
        eg-scores (reduce (fn [eg v]
                       (add-attr eg v :score
                                 (if (non-data v)
                                   (if (true-vertices v)
                                     (max 0.0 (min 1.0 (my-rand-gauss
                                                        (:TrueAprioriMean params)
                                                        (:TrueAprioriVariance params))))
                                     (max 0.0 (min 1.0 (my-rand-gauss
                                                        (:FalseAprioriMean params)
                                                        (:FalseAprioriVariance params)))))
                                   1.0)))
                     eg-conflicts (sort (nodes eg-conflicts)))]
    (if (empty? true-vertices) (random-expgraph-levels)
        {:expgraph eg-scores :true-vertices true-vertices :observations observations})))

(defn observation-groups
  [observations]
  (let [vs (vec (my-shuffle (sort observations)))
        split-locs (my-shuffle (range (count vs)))
        splits (partition-all 2 1 (sort (take (inc (:Steps params)) split-locs)))
        groups (if (empty? splits)
                 [(vec observations)]
                 (vec (map (fn [[pos1 pos2]] (subvec vs pos1 pos2))
                         ;; ensure the splits start with 0 and reach the end
                         (concat [[0 (second (first splits))]]
                                 (butlast (rest splits))
                                 [[(first (last splits)) (count vs)]]))))]
    (if (>= (:Steps params) (count groups))
      (vec (concat groups (repeat (inc (- (:Steps params) (count groups))) [])))
      groups)))

(defn generate-truedata
  []
  (let [{:keys [expgraph true-vertices observations]} (random-expgraph-levels)
        obs-groups (observation-groups observations)]
    {:training {:expgraph expgraph}
     :test obs-groups
     :expgraph expgraph
     :true-vertices true-vertices}))
