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

(defn new-observations
  []
  (set (repeatedly (* (:Steps params) 10) #(format "O%d" (my-rand-int 10000)))))

(defn new-explainers
  [vertices]
  (filter #(not (vertices %))
     (repeatedly 2 #(format "E%d" (my-rand-int 10000)))))

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

(defn random-expgraph
  []
  (loop [attempts 0]
    (let [all-obs (new-observations)
          expl-links (loop [unexp all-obs
                            vertices #{}
                            expl-links []]
                       (if (>= (count expl-links) (:NumExplainsLinks params))
                         (take (:NumExplainsLinks params) (my-shuffle expl-links))
                         ;; make some explainers for some of the unexplained
                         (let [to-be-explained (take (count unexp)
                                                     (my-shuffle (sort unexp)))
                               new-expl-links (mapcat
                                               (fn [v1] (make-explainers
                                                        vertices v1
                                                        (if (< attempts 10)
                                                          (:NumExplainers params) 0)))
                                               to-be-explained)
                               newly-explained (set (map second new-expl-links))
                               newly-unexplained (set (map first new-expl-links))
                               new-unexp (set/union (set/difference unexp newly-explained)
                                                newly-unexplained)
                               new-vertices (set/union vertices newly-unexplained)]
                           (recur new-unexp new-vertices
                                  (concat expl-links new-expl-links)))))
          eg (apply add-edges (digraph) expl-links)
          vs (sort (nodes eg))
          observations (sort ;; find the leaves (not just vertices
                        ;; from all-obs; some may have been
                        ;; linked in the graph
                        (filter (fn [v] (and (all-obs v)
                                       (empty? (neighbors eg v))
                                       (not-empty (incoming eg v))))
                           vs))
          observation-sets (loop [attempts 0]
                             (let [obs-choices (my-shuffle observations)
                                   obs (repeatedly
                                        (:UniqueTrueSets params)
                                        ;; about 3 observations per step (have
                                        ;; 10*steps to choose from, max)
                                        #(take (* 3 (:Steps params)) obs-choices))]
                               ;; try 20 times to get :UniqueTrueSets number of different true sets
                               (if (and (< attempts 20) (not= (:UniqueTrueSets params)
                                                              (count (set obs))))
                                 (recur (inc attempts)) obs)))
          ;; a true-set for each observation-set
          true-sets (if (not (dag? eg)) []
                        (for [os observation-sets]
                          (set (apply concat (for [v os] (arbitrary-path-up eg v))))))
          ;; different false-obs for each eg-score/true-set
          false-obs (for [true-set true-sets]
                      (filter #(not (true-set %)) observations))
          conflict-links (take (:NumConflictLinks params)
                               (my-shuffle
                                (sort #(let [c (compare (first %1) (first %2))]
                                         (if (= 0 c) (compare (second %1) (second %2)) c))
                                      (set (filter (fn [[v1 v2]]
                                                (and (not= v1 v2)
                                                     (or (not-any? #(% v1) true-sets)
                                                         (not-any? #(% v2) true-sets))
                                                     (or (not (all-obs v1))
                                                         (not (all-obs v2)))
                                                     (not (has-edge? eg v1 v2))
                                                     (not (has-edge? eg v2 v1))))
                                              (combinations vs 2))))))
          eg-conflicts (reduce set-conflicts eg conflict-links)
          ;; a different eg-score for each true-set
          eg-scores (let [vs (sort (nodes eg-conflicts))]
                      (for [[true-set f-obs] (partition 2 (interleave true-sets false-obs))]
                        (reduce (fn [eg v]
                             (add-attr eg v :score
                                       (cond (all-obs v) 1.0
                                             (true-set v)
                                             (max 0.0 (min 1.0 (my-rand-gauss
                                                                (:TrueAprioriMean params)
                                                                (:TrueAprioriVariance params))))
                                             ;; not true and explains
                                             ;; a false observation;
                                             ;; make it less likely
                                             (not-empty (set/intersection (neighbors eg v) (set f-obs)))
                                             (max 0.0 (min 1.0 (my-rand-gauss
                                                                (/ (:FalseAprioriMean params) 2.0)
                                                                (:FalseAprioriVariance params))))
                                             :else
                                             (max 0.0 (min 1.0 (my-rand-gauss
                                                                (:FalseAprioriMean params)
                                                                (:FalseAprioriVariance params)))))))
                           eg-conflicts vs)))]
      (if (empty? true-sets) (recur (inc attempts))
          {:expgraphs eg-scores
           :false-obs false-obs
           :observation-sets observation-sets
           :true-sets true-sets}))))

(defn observation-groups
  [observations]
  (let [vs (vec (my-shuffle (sort observations)))
        split-locs (my-shuffle (range (count vs)))
        splits (partition-all 2 1 (sort (take (inc (:Steps params)) split-locs)))
        groups (if (empty? splits)
                 [(vec observations)]
                 (vec (map (fn [[pos1 pos2]] (subvec vs pos1 pos2))
                         ;; ensure the splits start with 0 and reach the end
                         (filter (fn [[pos1 pos2]] (and pos1 pos2))
                            (concat [[0 (second (first splits))]]
                                    (butlast (rest splits))
                                    [[(first (last splits)) (count vs)]])))))]
    (if (>= (:Steps params) (count groups))
      (vec (concat groups (repeat (inc (- (:Steps params) (count groups))) [])))
      groups)))

(defn generate-truedata
  []
  (let [{:keys [expgraphs false-obs observation-sets true-sets]}
        ;; restrict the number of possible graphs
        (binding [rgen (new-seed (my-rand-int (:UniqueGraphs params)))]
          (random-expgraph))]
    ;; use the original random generator again
    (let [i (my-rand-int (count observation-sets))]
      {:training {:expgraph (nth expgraphs i)
                  :false-obs (nth false-obs i)}
       :expgraph (nth expgraphs i)
       :true-explainers (set (filter #(not ((set (nth observation-sets i)) %)) (nth true-sets i)))
       :true-obs (set (nth observation-sets i))
       :false-obs (nth false-obs i)
       :test (observation-groups (nth observation-sets i))})))
