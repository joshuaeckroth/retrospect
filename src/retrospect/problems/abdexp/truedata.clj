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
  (repeatedly (* (:Steps params) 3) #(format "O%d" (my-rand-int 10000))))

(defn new-explainers
  [vertices]
  (repeatedly
   (:NumExplainers params)
   #(format "E%d" (my-rand-int 10000))))

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
    (let [obs (new-observations)
          expl-links (loop [unexp (set obs)
                            vertices (set obs)
                            expl-links []]
                       (if (>= (count expl-links) (:NumExplainsLinks params))
                         expl-links
                         ;; make some explainers for some of the unexplained
                         (let [to-be-explained (take (count unexp)
                                                     (my-shuffle (sort unexp)))
                               new-expl-links (mapcat
                                               (fn [v1] (make-explainers
                                                        vertices v1
                                                        (if (< attempts 10) 1 0)))
                                               to-be-explained)
                               newly-explained (set (map second new-expl-links))
                               newly-unexplained (set (map first new-expl-links))
                               new-unexp (set/union (set/difference unexp newly-explained)
                                                newly-unexplained)
                               new-vertices (set/union vertices newly-unexplained)]
                           (recur new-unexp new-vertices
                                  (concat expl-links new-expl-links)))))
          eg (apply add-edges (digraph) expl-links)
          observations (set (filter #(and (empty? (neighbors eg %)) (not-empty (incoming eg %)))
                               (nodes eg)))
          true-vertices (if (not (dag? eg)) #{}
                            (set (apply concat (for [v observations]
                                                 (arbitrary-path-up eg v)))))
          conflict-links (let [vs (sort (nodes eg))]
                           (take (:NumConflictLinks params)
                                 (my-shuffle
                                  (sort #(let [c (compare (first %1) (first %2))]
                                           (if (= 0 c) (compare (second %1) (second %2)) c))
                                        (set (filter (fn [[v1 v2]]
                                                  (and (not= v1 v2)
                                                       (or (not (true-vertices v1))
                                                           (not (true-vertices v2)))
                                                       (not (has-edge? eg v1 v2))
                                                       (not (has-edge? eg v2 v1))))
                                                (combinations vs 2)))))))
          eg-conflicts (reduce set-conflicts eg conflict-links)
          eg-scores (reduce (fn [eg v]
                         (add-attr eg v :score
                                   (cond
                                    (observations v) 1.0
                                    (true-vertices v)
                                    (max 0.0 (min 1.0 (my-rand-gauss
                                                       (:TrueAprioriMean params)
                                                       (:TrueAprioriVariance params))))
                                    :else
                                    (max 0.0 (min 1.0 (my-rand-gauss
                                                       (:FalseAprioriMean params)
                                                       (:FalseAprioriVariance params)))))))
                       eg-conflicts (sort (nodes eg-conflicts)))]
      (if (empty? true-vertices) (recur (inc attempts))
          {:expgraph eg-scores :true-vertices true-vertices :observations observations}))))

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
  (let [{:keys [expgraph true-vertices observations]} (random-expgraph)
        obs-groups (observation-groups observations)]
    {:training {:expgraph expgraph}
     :test obs-groups
     :expgraph expgraph
     :true-vertices true-vertices}))
