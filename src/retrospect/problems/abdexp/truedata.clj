(ns retrospect.problems.abdexp.truedata
  (:use [clojure.set :only [difference]])
  (:use [loom.graph])
  (:use [loom.alg])
  (:use [loom.attr])
  (:use [clojure.contrib.combinatorics])
  (:require [clojure.set :as set])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.javabayes :only [build-bayesnet]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn arbitrary-path-up
  [expgraph true-values v]
  (loop [path [v]
         tv (assoc true-values v (rand-nth (sort (values expgraph v))))]
    (let [ns (incoming expgraph (last path))]
      (if (empty? ns) tv
          (let [expl (my-rand-nth (sort ns))]
            (recur (conj path expl)
                   (assoc tv expl (rand-nth (sort (values expgraph v))))))))))

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

(defn gen-parent-combinations
  [parent-vals]
  (if (empty? parent-vals) []
      (for [val (first parent-vals)]
        (conj (gen-parent-combinations (rest parent-vals)) val))))

(defn add-prob-table
  [expgraph vertex]
  (let [vals (sort (values expgraph vertex))
        parent-vals (map #(sort (values expgraph %)) (sort (explainers expgraph vertex)))
        parent-combs (let [pc (gen-parent-combinations parent-vals)]
                       (if (empty? pc) [[]] pc))
        probs-parent-combs-map (reduce (fn [m pc]
                                    (let [probs (repeatedly (count vals) my-rand)
                                          probs-sum (reduce + probs)]
                                      (assoc m pc (zipmap vals (map #(/ % probs-sum) probs)))))
                                  {} parent-combs)
        prob-table (for [v vals pc parent-combs]
                     (get-in probs-parent-combs-map [pc v]))]
    (add-attr expgraph vertex :probs prob-table)))

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

(defn gen-observation-sets
  [vertices]
  (loop [attempts 0]
    (let [obs-choices (take (* 10 (:Steps params)) (my-shuffle (sort vertices)))
          obs (repeatedly
               (:UniqueTrueSets params)
               ;; about 2 observations per step (have
               ;; 10*steps to choose from, max)
               #(take (* 2 (:Steps params)) obs-choices))]
      ;; try 20 times to get :UniqueTrueSets number of different true sets
      (if (and (< attempts 20) (not= (:UniqueTrueSets params)
                                     (count (set obs))))
        (recur (inc attempts)) obs))))

(defn gen-conflicts-links
  [true-values-maps eg vs]
  (let [vs-with-values (mapcat (fn [v] (map (fn [val] [v val]) (sort (values eg v)))) vs)]
    (take (:NumConflictLinks params)
          (my-shuffle
           (sort (fn [[[v1 val1] [v2 val2]] [[v3 val3] [v4 val4]]]
                   (let [c (compare (format "%s %s" v1 val1) (format "%s %s" v2 val2))]
                     (if (= 0 c) (compare (format "%s %s" v3 val3) (format "%s %s" v4 val4)) c)))
                 (set (filter (fn [[[v1 val1] [v2 val2]]]
                           (and (not= v1 v2)
                                (or (not-any? #(= (% v1) val1) true-values-maps)
                                    (not-any? #(= (% v2) val2) true-values-maps))
                                (not (has-edge? eg v1 v2))
                                (not (has-edge? eg v2 v1))))
                         (combinations vs-with-values 2))))))))

(defn random-scores
  [eg true-values v outgoing]
  (if (true-values v)
    (let [val (true-values v)
          p-true (max 0.0 (min 1.0 (my-rand-gauss
                                    (:TrueAprioriMean params)
                                    (:TrueAprioriVariance params))))
          p-false1 (* (my-rand) (- 1.0 p-true))
          p-false2 (- 1.0 p-true p-false1)]
      (assoc (zipmap (filter #(not= val %) (values eg v))
                     [p-false1 p-false2])
        (true-values v) p-true))
    (let [probs (repeatedly 3 #(my-rand))
          probs-sum (reduce + probs)
          probs-normalized (map #(/ % probs-sum) probs)]
      (zipmap (values eg v) probs-normalized))))

(defn gen-scores
  [expgraph vs true-values-maps]
  (for [true-values true-values-maps]
    (reduce (fn [eg v]
         (let [scores (random-scores eg true-values v (neighbors eg v))]
           (add-attr eg v :scores scores)))
       expgraph vs)))

(defn random-expgraph
  []
  (loop [attempts 0]
    (let [expl-links (gen-explains-links attempts)
          eg (apply add-edges (digraph) expl-links)
          vs (sort (nodes eg))
          ;; always three states ("values") for a vertex
          eg-values (reduce (fn [eg v]
                         (-> eg (add-attr v :id v)
                            (add-attr v :values ["1" "2" "3"])))
                       eg vs)
          observation-sets (gen-observation-sets vs)
          ;; a true-values map for each observation-set
          true-values-maps (if (not (dag? eg-values)) []
                               (for [os observation-sets]
                                 (reduce (fn [tv v] (arbitrary-path-up eg-values tv v)) {} os)))
          ;; different false-obs for each eg-score/true-set
          false-values-maps (for [true-values true-values-maps]
                              (reduce (fn [m v] (assoc m v (my-rand-nth (sort (values eg-values v)))))
                                 {} (filter #(not (true-values %)) vs)))
          conflict-links (gen-conflicts-links true-values-maps eg-values vs)
          eg-conflicts (reduce set-conflicts eg-values conflict-links)
          ;; a different eg-score for each true-set
          eg-scores (gen-scores eg-conflicts vs true-values-maps)
          eg-probs (map (fn [eg] (reduce add-prob-table eg vs)) eg-scores)
          bayesnets (map build-bayesnet eg-probs)]
      (if (empty? true-values-maps) (recur (inc attempts))
          {:expgraphs eg-probs
           :bayesnets bayesnets
           :observation-sets observation-sets
           :false-values-maps false-values-maps
           :true-values-maps true-values-maps}))))

(defn observation-groups
  [observations true-values]
  (let [vs (vec (my-shuffle (sort observations)))
        split-locs (my-shuffle (range (count vs)))
        splits (partition-all 2 1 (sort (take (inc (:Steps params)) split-locs)))
        groups (if (empty? splits)
                 [(vec observations)]
                 (vec (map (fn [[pos1 pos2]]
                           (vec (map (fn [v] [v (true-values v)])
                                   (subvec vs pos1 pos2))))
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
  (let [{:keys [expgraphs bayesnets observation-sets false-values-maps true-values-maps]}
        ;; restrict the number of possible graphs
        (binding [rgen (new-seed (my-rand-int (:UniqueGraphs params)))]
          (random-expgraph))]
    ;; use the original random generator again
    (let [i (my-rand-int (count observation-sets))]
      {:training {:expgraph (nth expgraphs i)
                  :bayesnet (nth bayesnets i)
                  :false-values-map (nth false-values-maps i)}
       :expgraph (nth expgraphs i)
       :bayesnet (nth bayesnets i)
       :true-values-map (nth true-values-maps i)
       :false-values-map (nth false-values-maps i)
       :test (doall (observation-groups (nth observation-sets i)
                                        (nth true-values-maps i)))})))
