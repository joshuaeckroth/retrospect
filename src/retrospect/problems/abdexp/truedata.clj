(ns retrospect.problems.abdexp.truedata
  (:use [clojure.set :only [difference]])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [clojure.contrib.combinatorics])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.random]))

(defn random-expgraph
  []
  (let [vertices (range 40)
        vs-repeated (apply concat (repeat 20 vertices))
        possible-expl-links (filter
                             #(< (second %) (first %))
                             (partition 2 (interleave (shuffle vs-repeated)
                                                      (shuffle vs-repeated))))
        expl-links (take (my-rand-int 10) (shuffle (set possible-expl-links)))
        eg (connect (apply add-edges (digraph) expl-links))
        eg-tops (reduce (fn [eg v] (add-edges eg [v v]))
                        eg (filter #(empty? (incoming eg %)) (nodes eg)))
        eg-filled (reduce fill eg-tops (filter #(empty? (neighbors eg-tops%))
                                               (nodes eg-tops)))
        non-leafs (set (filter #(not-empty (neighbors eg-filled %))
                               (nodes eg-filled)))
        bunch-of-edges (apply concat (repeat 20 (nodes eg-filled)))
        possible-conflict-links (partition 2 (interleave
                                              (shuffle bunch-of-edges)
                                              (shuffle bunch-of-edges)))
        valid-conflict-links (filter
                              (fn [[v1 v2]]
                                (and (< v1 v2) (non-leafs v1)
                                     (non-leafs v2)
                                     (not (has-edge? eg-filled v1 v2))
                                     (not (has-edge? eg-filled v2 v1))))
                              possible-conflict-links)
        conflict-links (take (my-rand-int 10) (shuffle valid-conflict-links))
        eg-conflicts (reduce set-conflicts eg-filled conflict-links)]
    eg-conflicts))

(defn find-least-cardinality
  [expgraph]
  (let [filled (filled-nodes expgraph)
        fillable (difference (nodes expgraph) filled)
        configs (mapcat (fn [n] (combinations fillable n))
                        (range (inc (count fillable))))
        egs (map (fn [config] (reduce fill expgraph config)) configs)
        complete (filter complete? egs)]
    (first (sort-by cardinality complete))))

(defn generate-truedata
  []
  (loop [expgraph (random-expgraph)]
    (let [least (find-least-cardinality expgraph)]
      (if (or (empty? (nodes expgraph)) (nil? least))
        (recur (random-expgraph))
        {:test expgraph :least least}))))
