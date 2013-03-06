(ns retrospect.candidacy
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [loom.graph])
  (:use [loom.attr])
  (:use [retrospect.random])
  (:use [clojure.contrib.combinatorics])
  (:require [retrospect.state :as state])
  (:use [retrospect.problems.abdexp.bayesnet]))

(defn add-prob-table
  [expgraph vertex]
  (let [vals (sort (values expgraph vertex))
        parent-vals (map (fn [v] (map (fn [val] [v val]) (sort (values expgraph v))))
                       (sort (explainers expgraph vertex)))
        parent-combs (let [pc (gen-parent-combinations parent-vals)]
                       (if (empty? pc) [#{}] pc))
        probs-parent-combs-map
        (reduce (fn [m pc]
             (let [probs-orig (sort (repeatedly (count vals) my-rand))
                   probs (my-shuffle (concat [(/ (first probs-orig) 2.0)]
                                             (butlast (rest probs-orig))
                                             [(* 2.0 (last probs-orig))]))
                   probs-sum (reduce + probs)
                   probs-pairs (interleave vals (map #(/ % probs-sum) probs))]
               (assoc m pc (apply sorted-map probs-pairs))))
           {} parent-combs)
        prob-table (for [v vals pc parent-combs]
                     (get-in probs-parent-combs-map [pc v]))]
    (add-attr expgraph vertex :probs {:table prob-table :map probs-parent-combs-map})))

(defn observe-evidence
  [bn]
  (unobserve-all bn)
  (observe-seq bn [["O1" "true"] ["O2" "true"]]))

(defn make-net
  []
  (let [expgraph (-> (digraph)
                    (add-edges ["C1" "O1"])
                    (add-edges ["C2" "O1"])
                    (add-edges ["C3" "O2"])
                    (add-attr "C1" :values ["true" "false"])
                    (add-attr "C2" :values ["true" "false"])
                    (add-attr "C3" :values ["true" "false"])
                    (add-attr "O1" :values ["true" "false"])
                    (add-attr "O2" :values ["true" "false"]))
        eg-probs (reduce add-prob-table expgraph (sort (nodes expgraph)))
        bayesnet (build-bayesnet eg-probs)]
    {:expgraph eg-probs :bayesnet bayesnet}))

(defn mpe
  [bn]
  (observe-evidence bn)
  (let [{:keys [states prob]} (most-probable-explanation bn)]
    [(set (map (fn [[k v]] [k v])
             (filter #(and (not= "O1" (first %)) (not= "O2" (first %))) states)))
     prob]))

(defn mre-bayes-factor
  [bn pairs]
  (let [post (do (observe-evidence bn)
                 (get-posterior bn pairs))
        prior (do (unobserve-all bn)
                  (get-posterior bn pairs))]
    (cond (= 0.0 prior) 0.0
          (= 1.0 post prior) 0.0
          (and (< prior 1.0) (= 1.0 post)) 1e20 ;; infinity; definitely the MRE
          :else
          (/ (* post (- 1.0 prior)) (* prior (- 1.0 post))))))

(defn cause-combination-posteriors
  [bn]
  (let [causes (map (fn [c] [[c "true"] [c "false"]]) ["C1" "C2" "C3"])
        cause-groups (for [n [1 2 3] c (combinations causes n)]
                       (gen-parent-combinations c))]
    (set (apply concat (map #(sort-by first %) cause-groups)))))

(defn decampos
  [bn]
  (let [causes (map (fn [c] [[c "true"] [c "false"]]) ["C1" "C2" "C3"])
        parent-combs (gen-parent-combinations causes)]
    (set (apply concat
                (for [cs parent-combs]
                  (let [p (do (unobserve-all bn)
                              (observe-seq bn cs)
                              (get-posterior bn [["O1" "true"] ["O2" "true"]]))
                        subexps (set (map set (apply concat
                                                   (for [n (range 1 (inc (count cs)))]
                                                     (combinations cs n)))))]
                    (filter (fn [[se psub]] (and (> (Math/abs (- psub p)) 0.01)
                                           (> psub p)))
                       (map (fn [se] (do (unobserve-all bn)
                                      (observe-seq bn se)
                                      [se (get-posterior bn [["O1" "true"] ["O2" "true"]])]))
                          subexps))))))))

(defn aifw
  [bn]
  (observe-evidence bn)
  (if (> (get-posterior bn [["C1" "true"]])
         (get-posterior bn [["C2" "true"]]))
    #{["C1" "true"] ["C3" "true"]}
    #{["C2" "true"] ["C3" "true"]}))

(defn -main
  []
  (dosync (alter state/batch (constantly true)))
  (binding [rgen (new-seed 0)]
    (loop [i 0
           counts {:mpe 0 :map 0 :mre 0 :decampos 0}]
      (if (= i 10000)
        (println counts)
        (let [{:keys [expgraph bayesnet]} (make-net)
              abd (aifw bayesnet)
              mpe (mpe bayesnet)
              calcs (reduce (fn [m pairs]
                         (assoc m pairs {:map (get-posterior bayesnet pairs)
                                         :gbf (mre-bayes-factor bayesnet pairs)}))
                       {} (cause-combination-posteriors bayesnet))
              map (let [[c m] (last (sort-by (comp :map second) calcs))] [c (:map m)])
              mre (let [[c m] (last (sort-by (comp :gbf second) calcs))] [c (:gbf m)])
              decampos (last (sort-by second (decampos bayesnet)))]
          (comment
            (println "abd:" abd)
            (println "mpe:" mpe)
            (println "map:" map)
            (println "mre:" mre)
            (println "decampos:" decampos)
            (println))
          (recur (inc i)
                 (-> counts
                    (update-in [:mpe] #(if (= abd (first mpe)) (inc %) %))
                    (update-in [:map] #(if (= abd (first map)) (inc %) %))
                    (update-in [:mre] #(if (= abd (first mre)) (inc %) %))
                    (update-in [:decampos] #(if (= abd (first decampos)) (inc %) %)))))))))

