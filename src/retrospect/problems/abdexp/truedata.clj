(ns retrospect.problems.abdexp.truedata
  (:use [clojure.set :only [difference]])
  (:use [loom.io :only [view]])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr])
  (:use [clojure.contrib.combinatorics])
  (:require [clojure.set :as set])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.bayesnet :only [build-bayesnet]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn new-explainers
  [vertices]
  (filter #(not (vertices %))
     (repeatedly 2 #(format "V%d" (my-rand-int 10000)))))

(defn reuse-explainers
  [vertices n]
  (take n (my-shuffle (sort vertices))))

(defn make-explainers
  [vertices v1 reuse-n]
  (map (fn [v2] [v2 v1])
     (take (inc (my-rand-int (:NumExplainers params)))
           (my-shuffle
            (concat (new-explainers vertices)
                    (reuse-explainers vertices reuse-n))))))

(defn gen-explains-links
  [attempts]
  (let [initial-vertices (set (repeatedly (* (:Steps params) 2)
                                          #(format "V%d" (my-rand-int 10000))))]
    (loop [unexp initial-vertices
           vs #{}
           expl-links []]
      (if (>= (count expl-links) (:NumExplainsLinks params))
        (take (:NumExplainsLinks params) (my-shuffle expl-links))
        ;; make some explainers for some of the unexplained
        (let [to-be-explained (take (count unexp) (my-shuffle (sort unexp)))
              reuse-n (if (< attempts 10) (:NumExplainers params) 0)
              new-expl-links (mapcat (fn [v] (make-explainers vs v reuse-n))
                                     to-be-explained)
              newly-explained (set (map second new-expl-links))
              newly-unexplained (set (map first new-expl-links))
              new-unexp (set/union (set/difference unexp newly-explained) newly-unexplained)
              new-vs (set/union vs newly-unexplained)]
          (recur new-unexp new-vs (concat expl-links new-expl-links)))))))

(defn arbitrary-path-up
  [expgraph true-values v]
  (loop [path [v]
         tv (assoc true-values v (my-rand-nth (sort (values expgraph v))))]
    (let [vs (incoming expgraph (last path))]
      (if (empty? vs) tv
          (let [expl (my-rand-nth (sort vs))]
            (recur (conj path expl)
                   (assoc tv expl (my-rand-nth (sort (values expgraph v))))))))))

(defn gen-conflicts-links
  [eg vs path]
  (let [vs-with-values (mapcat (fn [v] (map (fn [val] [v val]) (sort (values eg v)))) vs)]
    (take (:NumConflictLinks params)
          (my-shuffle
           (sort (fn [[[v1 val1] [v2 val2]] [[v3 val3] [v4 val4]]]
                   (let [c (compare (format "%s %s" v1 val1) (format "%s %s" v2 val2))]
                     (if (= 0 c) (compare (format "%s %s" v3 val3) (format "%s %s" v4 val4)) c)))
                 (set (filter (fn [[[v1 val1] [v2 val2]]]
                           (and (not= v1 v2)
                                (or (not= (path v1) val1) (not= (path v2) val2))
                                (not (has-edge? eg v1 v2))
                                (not (has-edge? eg v2 v1))))
                         (combinations vs-with-values 2))))))))

(defn add-prob-table
  [expgraph vertex]
  (let [vals (sort (values expgraph vertex))
        parent-vals (map (fn [v] (map (fn [val] [v val]) (sort (values expgraph v))))
                       (sort (explainers expgraph vertex)))
        parent-combs (let [pc (gen-parent-combinations parent-vals)]
                       (if (empty? pc) [#{}] pc))
        probs-parent-combs-map
        (reduce (fn [m pc]
             (let [probs (repeatedly (count vals) my-rand)
                   probs-sum (reduce + probs)
                   probs-pairs (interleave vals (map #(/ % probs-sum) probs))]
               (assoc m pc (apply sorted-map probs-pairs))))
           {} parent-combs)
        prob-table (for [v vals pc parent-combs]
                     (get-in probs-parent-combs-map [pc v]))]
    (add-attr expgraph vertex :probs {:table prob-table :map probs-parent-combs-map})))

(defn sample-expgraph
  "Generate a true-values map based on sampling from the network."
  [expgraph]
  (letfn [(any-incompatible? [true-values-map]
            (some (fn [[v1 val1]] (some (fn [[v2 val2]]
                                   (conflicts? expgraph [v1 val1] [v2 val2]))
                                 (seq true-values-map)))
               (seq true-values-map)))]
    (loop [vs (reverse (sorted-by-dep expgraph))
           true-values-map {}]
      (if (empty? vs) true-values-map
          (let [v (first vs)
                parents (explainers expgraph v)
                parent-vals (set (map (fn [v] [v (get true-values-map v)]) parents))
                val-probs (sort-by second (map (fn [val] [val (prob expgraph v val parent-vals)])
                                             (values expgraph v)))
                rand-prob (my-rand)
                chosen-val (ffirst (drop-while #(< (second %) rand-prob)
                                               (reductions (fn [[val1 prob1] [val2 prob2]]
                                                             [val2 (+ prob1 prob2)]) val-probs)))
                new-true-values-map (conj true-values-map [v chosen-val])]
            ;; if an incompatibility has been introduced, start over
            (if (any-incompatible? new-true-values-map)
              (recur (reverse (sorted-by-dep expgraph)) {})
              (recur (rest vs) new-true-values-map)))))))

(defn rand-vals
  []
  (vec (map #(format "S%d" %) (range 1 (inc (my-rand-nth (range 2 (inc (:MaxStates params)))))))))

(defn random-expgraph
  []
  (loop [attempts 0]
    (let [expl-links (gen-explains-links attempts)
          eg (apply add-edges (digraph) expl-links)]
      (if (not (dag? eg))
        (recur (inc attempts))
        (let [vs-tmp (sort (nodes eg))
              eg-values (reduce (fn [eg v]
                             (-> eg (add-attr v :id v)
                                (add-attr v :values (rand-vals))))
                           eg vs-tmp)
              ;; a path gives selects vertex-value pairs from all bottom
              ;; vertices to the top to ensure that conflicts links do not
              ;; disable all possible paths
              path (reduce (fn [tv v] (arbitrary-path-up eg-values tv v))
                      {} (bottom-nodes eg-values))
              conflict-links (gen-conflicts-links eg-values vs-tmp path)
              eg-conflicts (reduce set-conflicts eg-values conflict-links)
              ;; netica restriction
              nodes-with-conflicts (+ (count vs-tmp) (count conflict-links))
              eg-reduced (if (> nodes-with-conflicts 30)
                           (let [vs (set (take (- 30 (count conflict-links))
                                               (my-shuffle vs-tmp)))]
                             (apply remove-nodes eg-conflicts (filter #(not (vs %)) vs-tmp)))
                           eg-conflicts)
              vs (sort (nodes eg-reduced))
              eg-probs (reduce add-prob-table eg-reduced vs)
              bayesnet (build-bayesnet eg-probs)
              true-values-map (sample-expgraph eg-probs)
              observations (take (:Steps params)
                                 (my-shuffle (sort-by first (seq true-values-map))))]
          {:expgraph eg-probs
           :bayesnet bayesnet
           :observations observations
           :true-values-map true-values-map})))))

(defn observation-groups
  [observations]
  (let [obs-vec (vec (my-shuffle (sort observations)))
        split-locs (my-shuffle (range (count obs-vec)))
        splits (partition-all 2 1 (sort (take (inc (:Steps params)) split-locs)))
        groups (if (empty? splits)
                 [(vec observations)]
                 (vec (map (fn [[pos1 pos2]] (subvec obs-vec pos1 pos2))
                         ;; ensure the splits start with 0 and reach the end
                         (filter (fn [[pos1 pos2]] (and pos1 pos2))
                            (concat [[0 (second (first splits))]]
                                    (butlast (rest splits))
                                    [[(first (last splits)) (count obs-vec)]])))))]
    (if (>= (:Steps params) (count groups))
      (vec (concat groups (repeat (inc (- (:Steps params) (count groups))) [])))
      groups)))

(defn generate-truedata
  []
  (let [{:keys [expgraph bayesnet observations true-values-map]}
        ;; restrict the number of possible graphs
        (binding [rgen (new-seed (my-rand-int (:UniqueGraphs params)))]
          (random-expgraph))]
    {:training {:expgraph expgraph
                :bayesnet bayesnet}
     :expgraph expgraph
     :bayesnet bayesnet
     :true-values-map true-values-map
     :test (doall (observation-groups observations))}))
