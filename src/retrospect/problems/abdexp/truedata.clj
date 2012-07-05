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
  (let [observations (set (repeatedly (* 10 (:Steps params))
                                      #(format "O%d" (my-rand-int 1000))))
        vertices (set (repeatedly (:NumVertices params)
                                  #(format "E%d" (+ 1000 (my-rand-int 1000)))))
        vs-levels (loop [vs-levels []
                         vs vertices
                         cuts (repeatedly (:NumLevels params)
                                          #(inc (my-rand-int (/ (:NumVertices params)
                                                                (:NumLevels params)))))]
                    (if (empty? cuts) (conj (vec vs-levels) observations)
                        (recur (conj vs-levels (take (first cuts) vs))
                               (drop (first cuts) vs)
                               (rest cuts))))
        expl-links (set (mapcat (fn [[vs1 vs2]]
                                  (mapcat (fn [v1]
                                            (map (fn [v2] [v1 v2])
                                               (take (inc (my-rand-int (:MaxExplainLinks params)))
                                                     (my-shuffle vs2))))
                                          vs1))
                                (partition 2 1 vs-levels)))
        eg (apply add-edges (digraph) expl-links)
        eg-filled (reduce fill eg (last vs-levels))
        true-vertices (set (apply concat (for [v (filled-nodes eg-filled)]
                                           (arbitrary-path-up eg-filled v))))
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
        eg-forced (apply force-fill eg-scores observations)]
    (if (empty? (forced-nodes eg-forced)) (random-expgraph-levels)
        {:expgraph eg-forced :true-vertices true-vertices})))

(defn segment-expgraph-steps
  [expgraph]
  (let [observations (forced-nodes expgraph)
        obs-groups (let [vs (vec (my-shuffle (sort observations)))
                         split-locs (my-shuffle (range (count vs)))
                         splits (partition 2 1 (sort (take (inc (:Steps params)) split-locs)))]
                     (vec (map (fn [[pos1 pos2]] (subvec vs pos1 pos2)) splits)))]
    (loop [time 1
           expgraphs {}]
      (if (> time (:Steps params)) expgraphs
          (let [prior-expgraph (get expgraphs (dec time))
                prior-vertices (if prior-expgraph
                                 (set (sorted-by-dep prior-expgraph)) #{})
                obs-up-to-time (if (>= (count obs-groups) time)
                                 (mapcat #(nth obs-groups %) (range time)) [])
                available-vertices (filter #(not (prior-vertices %))
                                      (sorted-by-dep expgraph obs-up-to-time))
                ;; don't take all newly-observed vertices and explainers
                new-vertices (take (my-rand-int (count available-vertices))
                                   available-vertices)
                current-vertices (set/union (set prior-vertices) (set new-vertices))
                related-edges (concat (mapcat (fn [v1]
                                              (map (fn [v2] [v1 v2])
                                                 (filter current-vertices (neighbors expgraph v1))))
                                            new-vertices)
                                      (mapcat (fn [v2]
                                                (map (fn [v1] [v1 v2])
                                                   (filter current-vertices (incoming expgraph v2))))
                                              new-vertices))
                new-edges (if prior-expgraph
                            (filter #(not (apply has-edge? prior-expgraph %))
                               related-edges)
                            related-edges)
                new-explains (filter #(not (apply conflicts? expgraph %)) new-edges)
                new-conflicts (filter #(apply conflicts? expgraph %) new-edges)
                next-expgraph-edges (apply add-edges (or prior-expgraph (digraph)) new-edges)
                next-expgraph-scores (reduce (fn [eg v]
                                          (add-attr eg v :score (score expgraph v)))
                                        next-expgraph-edges new-vertices)
                next-expgraph-conflicts (apply set-conflicts next-expgraph-scores new-conflicts)
                next-expgraph-filled (apply force-fill next-expgraph-conflicts
                                            (filter observations new-vertices))]
            (recur (inc time) (assoc expgraphs time next-expgraph-filled)))))))

(defn generate-truedata
  []
  (let [{:keys [expgraph true-vertices observations]} (random-expgraph-levels)
        expgraphs (segment-expgraph-steps expgraph)]
    {:training {:test expgraphs
                :true-vertices true-vertices}
     :test expgraphs
     :true-vertices true-vertices}))
