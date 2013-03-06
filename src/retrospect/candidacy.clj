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
             (let [probs (my-shuffle (repeatedly (count vals) my-rand))
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

(defn natural-bayesnet?
  "A Bayesnet is considered 'natural' if O1 is more probable if either
   C1 or C2 is true, and O2 is more probable if C3 is true."
  [bn]
  (let [o1-prior (do (unobserve-all bn)
                     (get-posterior bn [["O1" "true"]]))
        o1-c1 (do (unobserve-all bn)
                  (observe-seq bn [["C1" "true"]])
                  (get-posterior bn [["O1" "true"]]))
        o1-c2 (do (unobserve-all bn)
                  (observe-seq bn [["C2" "true"]])
                  (get-posterior bn [["O1" "true"]]))
        o2-prior (do (unobserve-all bn)
                     (get-posterior bn [["O2" "true"]]))
        o2-c3 (do (unobserve-all bn)
                  (observe-seq bn [["C3" "true"]])
                  (get-posterior bn [["O2" "true"]]))]
    (and (or (> o1-c1 o1-prior)
             (> o1-c2 o1-prior))
         (> o2-c3 o2-prior))))

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

(def cause-combination-posteriors
  (memoize
   (fn [bn]
     (let [causes (map (fn [c] [[c "true"] [c "false"]]) ["C1" "C2" "C3"])
           cause-groups (for [n [1 2 3] c (combinations causes n)]
                          (gen-parent-combinations c))]
       (set (apply concat (map #(sort-by first %) cause-groups)))))))

(defn decampos
  [bn]
  (let [causes (map (fn [c] [[c "true"] [c "false"]]) ["C1" "C2" "C3"])
        parent-combs (gen-parent-combinations causes)
        probs (set (apply concat
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
                                    subexps))))))]
    (last (sort-by second probs))))

(defn bayes-map
  [bn]
  (observe-evidence bn)
  (let [causes (cause-combination-posteriors bn)
        maps (for [c causes] [c (get-posterior bn c)])]
    (last (sort-by second maps))))

(defn bayes-map-gardenfors
  [bn]
  (observe-evidence bn)
  (let [causes (cause-combination-posteriors bn)
        maps (for [c causes] [c (get-posterior bn c)])]
    (last (sort-by second (filter (fn [[c _]]
                               (do (unobserve-all bn)
                                   (observe-seq bn c)
                                   (let [post-ex (get-posterior bn [["O1" "true"] ["O2" "true"]])]
                                     (unobserve-all bn)
                                     (let [prior-ev (get-posterior bn [["O1" "true"] ["O2" "true"]])
                                           prior-ex (do (unobserve-all bn)
                                                        (get-posterior bn c))]
                                       (and (> post-ex prior-ex) (< prior-ex 1.0))))))
                             maps)))))

(defn mre
  [bn]
  (observe-evidence bn)
  (let [causes (cause-combination-posteriors bn)
        gbfs (for [c causes] [ c (mre-bayes-factor bn c)])]
    (last (sort-by second gbfs))))

(defn aifw
  [bn]
  (observe-evidence bn)
  (if (> (get-posterior bn [["C1" "true"]])
         (get-posterior bn [["C2" "true"]]))
    #{["C1" "true"] ["C3" "true"]}
    #{["C2" "true"] ["C3" "true"]}))

(defn do-experiment
  [iters]
  (dosync (alter state/batch (constantly true)))
  (binding [rgen (new-seed 0)]
    (loop [i 0
           counts {:natural 0 :total 0}]
      (if (= i iters) counts
          (let [{:keys [expgraph bayesnet]} (make-net)
                natural? (natural-bayesnet? bayesnet)
                abd (aifw bayesnet)
                mpe (mpe bayesnet)
                map (bayes-map bayesnet)
                map-gardenfors (bayes-map-gardenfors bayesnet)
                mre (mre bayesnet)
                decampos (decampos bayesnet)
                update-counts (fn [counts key result]
                                (let [c (if (nil? (get-in counts [natural? key]))
                                          (assoc-in counts [natural? key] 0)
                                          counts)]
                                  (update-in c [natural? key] #(if (= abd (first result)) (inc %) %))))]
            (comment
              (println "natural?" natural?)
              (println "abd:" abd)
              (println "mpe:" mpe)
              (println "map:" map)
              (println "map-gardenfors:" map-gardenfors)
              (println "mre:" mre)
              (println "decampos:" decampos)
              (println))
            (recur (inc i)
                   (-> (if natural?
                        (-> counts
                           (update-in [:total] inc)
                           (update-in [:natural] inc))
                        (update-in counts [:total] inc))
                      (update-counts :mpe mpe)
                      (update-counts :map map)
                      (update-counts :map-gardenfors map-gardenfors)
                      (update-counts :mre mre)
                      (update-counts :decampos decampos))))))))

(defn -main
  []
  (let [results (do-experiment 1000)]
    (println "Total:" (get results :total))
    (doseq [[k v] (get results false)]
      (println (format "%20s   %5d (%.2f%%)" (name k) v (double (* 100.0 (/ v (get results :total)))))))
    (println)
    (println (format "Natural: %d (%.2f%%)"
                (get results :natural)
                (double (* 100.0 (/ (get results :natural)
                                    (get results :total))))))
    (doseq [[k v] (get results true)]
      (println (format "%20s   %5d (%.2f%%)" (name k) v (double (* 100.0 (/ v (get results :natural)))))))
    (println)))
