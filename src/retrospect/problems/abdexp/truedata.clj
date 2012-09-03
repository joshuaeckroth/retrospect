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
  (let [[true-obs false-obs]
        (split-at (* (:Steps params) 5)
                  (repeatedly (* (:Steps params) 10) #(format "O%d" (my-rand-int 10000))))]
    [(set true-obs) (set false-obs)]))

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
    (let [[true-obs false-obs] (new-observations)
          all-obs (set (concat true-obs false-obs))
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
          observation-sets (repeatedly
                            (:UniqueTrueSets params)
                            #(set (take (* 2 (:Steps params))
                                        (my-shuffle
                                         (sort
                                          (filter (fn [n] (and (true-obs n)
                                                         (empty? (neighbors eg n))
                                                         (not-empty (incoming eg n))))
                                             (nodes eg)))))))
          true-sets (if (not (dag? eg)) []
                        (for [os observation-sets]
                          (set (apply concat (for [v os] (arbitrary-path-up eg v))))))
          conflict-links (let [vs (sort (nodes eg))]
                           (take (:NumConflictLinks params)
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
                                                (combinations vs 2)))))))
          eg-conflicts (reduce set-conflicts eg conflict-links)
          eg-scores (reduce (fn [eg v]
                         (add-attr eg v :score
                                   (cond
                                    ((set (concat true-obs false-obs)) v) 1.0
                                    (some #(% v) true-sets)
                                    (max 0.0 (min 1.0 (my-rand-gauss
                                                       (:TrueAprioriMean params)
                                                       (:TrueAprioriVariance params))))
                                    :else
                                    (max 0.0 (min 1.0 (my-rand-gauss
                                                       (:FalseAprioriMean params)
                                                       (:FalseAprioriVariance params)))))))
                       eg-conflicts (sort (nodes eg-conflicts)))]
      (if (empty? true-sets) (recur (inc attempts))
          {:expgraph eg-scores
           :false-obs (filter false-obs (nodes eg-scores))
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
  ;; restrict the number of possible graphs
  (let [[truedata observation-sets true-sets]
        (binding [rgen (new-seed (my-rand-int (:UniqueGraphs params)))]
          (let [{:keys [expgraph false-obs observation-sets true-sets]} (random-expgraph)]
            [{:training {:expgraph expgraph
                         :false-obs false-obs}
              :expgraph expgraph}
             observation-sets
             true-sets]))]
    ;; use the original random generator again
    (let [i (my-rand-int (count observation-sets))]
      (assoc truedata
        :true-explainers (set (filter (comp not (nth observation-sets i)) (nth true-sets i)))
        :true-obs (set (apply concat observation-sets))
        :test (observation-groups (nth observation-sets i))))))
