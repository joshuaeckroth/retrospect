(ns retrospect.problems.abdexp.truedata
  (:use [clojure.set :only [difference]])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr])
  (:use [clojure.contrib.combinatorics])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random])
  (:use [retrospect.state]))

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
        eg-tops (reduce (fn [eg v] (add-edges eg [v v]))
                        eg (first vs-levels))
        eg-filled (reduce fill eg-tops (last vs-levels))
        non-leaves (set (filter #(not-empty (neighbors eg-filled %))
                                (nodes eg-filled)))
        conflict-links (take (:MaxConflictLinks params)
                             (set (mapcat (fn [vs]
                                            (filter (fn [[v1 v2]] (and (not= v1 v2)
                                                                       (not (has-edge? eg v1 v2))
                                                                       (not (has-edge? eg v2 v1))))
                                                    (partition 2 (interleave (my-shuffle vs)
                                                                             (my-shuffle vs)))))
                                          (butlast vs-levels))))
        eg-conflicts (reduce set-conflicts eg-filled conflict-links)
        eg-scores (reduce (fn [eg v]
                            (add-attr eg v :score
                                      (cond (and (:Scores params)
                                                 (non-leaves v))
                                            (my-rand)
                                            (and (not (:Scores params))
                                                 (non-leaves v))
                                            1.0
                                            ;; leaf
                                            :else 0.0)))
                          eg-conflicts (sort (nodes eg-conflicts)))]
    eg-scores))

(defn random-expgraph
  []
  (let [vertices (range (:NumVertices params))
        vs-repeated (apply concat (repeat (/ (:NumVertices params) 2) vertices))
        possible-expl-links (filter
                             #(< (second %) (first %))
                             (partition 2 (interleave (my-shuffle vs-repeated)
                                                      (my-shuffle vs-repeated))))
        expl-links (set (take (my-rand-int (:MaxExplainLinks params))
                              (my-shuffle possible-expl-links)))
        eg (connect (apply add-edges (digraph) expl-links))
        eg-tops (reduce (fn [eg v] (add-edges eg [v v]))
                        eg (filter #(empty? (explainers eg %)) (nodes eg)))
        eg-filled (reduce fill eg-tops (filter #(empty? (neighbors eg-tops %))
                                               (nodes eg-tops)))
        non-leaves (set (filter #(not-empty (neighbors eg-filled %))
                                (nodes eg-filled)))
        bunch-of-edges (apply concat (repeat (/ (:NumVertices params) 2)
                                             (sort (nodes eg-filled))))
        possible-conflict-links (partition 2 (interleave
                                              (my-shuffle bunch-of-edges)
                                              (my-shuffle bunch-of-edges)))
        valid-conflict-links (filter
                              (fn [[v1 v2]]
                                (and (< v1 v2) (non-leaves v1)
                                     (non-leaves v2)
                                     (not (has-edge? eg-filled v1 v2))
                                     (not (has-edge? eg-filled v2 v1))))
                              possible-conflict-links)
        conflict-links (take (my-rand-int (:MaxConflictLinks params))
                             (my-shuffle valid-conflict-links))
        eg-conflicts (reduce set-conflicts eg-filled conflict-links)
        eg-scores (reduce (fn [eg v]
                            (add-attr eg v :score
                                      (cond (and (:Scores params)
                                                 (non-leaves v))
                                            (my-rand)
                                            (and (not (:Scores params))
                                                 (non-leaves v))
                                            1.0
                                            ;; leaf
                                            :else 0.0)))
                          eg-conflicts (sort (nodes eg-conflicts)))]
    eg-scores))

(defn find-least
  [expgraph]
  (let [filled (filled-nodes expgraph)
        fillable (difference (nodes expgraph) filled)
        configs (mapcat (fn [n] (combinations fillable n))
                        (range (inc (count fillable))))
        egs (map (fn [config] (reduce fill expgraph config)) configs)
        complete (filter complete? egs)]
    (first (sort-by composite-score complete))))

(defn generate-truedata
  []
  (loop [expgraph (random-expgraph-levels)]
    (let [least (find-least expgraph)]
      (if (or (empty? (nodes expgraph)) (nil? least))
        (recur (random-expgraph))
        {:test expgraph :least least}))))
