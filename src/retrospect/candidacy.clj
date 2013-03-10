(ns retrospect.candidacy
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [loom.graph])
  (:use [loom.attr])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
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

(defn valid-bayesnet?
  "A Bayesnet is considered 'valid' if C1 is more probable than C2 (posterior)."
  [bn]
  (observe-evidence bn)
  (let [c1-post (get-posterior bn [["C1" "true"]])
        c2-post (get-posterior bn [["C2" "true"]])]
    (> c1-post c2-post)))

(defn causal-bayesnet?
  [bn]
  (let [causes? (fn [caused not-caused] (> caused not-caused))
        o1-not-caused (do (unobserve-all bn)
                          (observe-seq bn [["C1" "false"] ["C2" "false"]])
                          (get-posterior bn [["O1" "true"]]))
        o2-not-caused (do (unobserve-all bn)
                          (observe-seq bn [["C3" "false"]])
                          (get-posterior bn [["O2" "true"]]))
        o1-c1 (do (unobserve-all bn)
                  (observe-seq bn [["C1" "true"]])
                  (get-posterior bn [["O1" "true"]]))
        o1-c2 (do (unobserve-all bn)
                  (observe-seq bn [["C2" "true"]])
                  (get-posterior bn [["O1" "true"]]))
        o2-c3 (do (unobserve-all bn)
                  (observe-seq bn [["C3" "true"]])
                  (get-posterior bn [["O2" "true"]]))]
    (and (causes? o1-c1 o1-not-caused)
         (causes? o1-c2 o1-not-caused)
         (causes? o2-c3 o2-not-caused))))

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
   (fn []
     (let [causes (map (fn [c] [[c "true"] [c "false"]]) ["C1" "C2" "C3"])
           cause-groups (for [n [1 2 3] c (combinations causes n)]
                          (gen-parent-combinations c))]
       (set (apply concat (map #(sort-by first %) cause-groups)))))))

(defn decampos
  "indep? is true if using independence-based simplification, false if
   using relevance-based simplification."
  [bn indep?]
  (let [epsilon 0.05
        simplification? (fn [p psub] (if indep?
                                      ;; independence-based simplification
                                      (and (>= psub (* (- 1.0 epsilon) p))
                                           (<= psub (* (+ 1.0 epsilon) p)))
                                      ;; relevance-based simplification
                                      (>= psub (* (- 1.0 epsilon) p))))
        mpe (first (mpe bn))
        p (do (unobserve-all bn)
              (observe-seq bn mpe)
              (get-posterior bn [["O1" "true"] ["O2" "true"]]))
        subexps (set (map set (apply concat
                                   (for [n (range 1 (inc (count mpe)))]
                                     (combinations mpe n)))))
        subexps-probs (map (fn [se] (do (unobserve-all bn)
                                     (observe-seq bn se)
                                     [se (get-posterior bn [["O1" "true"] ["O2" "true"]])]))
                         subexps)
        subexps-simp (filter (fn [[se psub]] (simplification? p psub)) subexps-probs)]
    ;; now find the best simplification of cs
    (first (filter (fn [[se psub]]
                (every? (fn [[se2 psub2]]
                          (or
                           ;; every other simplifcation has more elements
                           (> (count se2) (count se))
                           ;; or if it has same number of elements, it's further from original prob
                           (and (= (count se2) (count se))
                                (>= (Math/abs (- psub2 p)) (Math/abs (- psub p))))))
                        subexps-simp))
              subexps-simp))))

(defn bayes-map-gardenfors
  [bn exp?]
  (observe-evidence bn)
  (let [causes (cause-combination-posteriors)
        maps (for [c causes]
               (let [prior (do (unobserve-all bn)
                               (get-posterior bn [["O1" "true"] ["O2" "true"]]))
                     post (do (unobserve-all bn)
                              (observe-seq bn c)
                              (get-posterior bn [["O1" "true"] ["O2" "true"]]))]
                 [c (if exp? (/ post prior)
                        (do (unobserve-all bn)
                            (get-posterior bn c)))]))
        exps (filter (fn [[c _]]
                  (do (unobserve-all bn)
                      (observe-seq bn c)
                      (let [post-ex (get-posterior bn [["O1" "true"] ["O2" "true"]])]
                        (unobserve-all bn)
                        (let [prior-ev (get-posterior bn [["O1" "true"] ["O2" "true"]])
                              prior-ex (do (unobserve-all bn)
                                           (get-posterior bn c))]
                          (> post-ex prior-ex)))))
                maps)]
    (or (last (sort-by second exps))
        [#{} 0.0])))

(defn mre
  [bn]
  (observe-evidence bn)
  (let [causes (cause-combination-posteriors)
        gbfs (for [c causes] [ c (mre-bayes-factor bn c)])]
    (or (last (sort-by second gbfs))
        [#{} 0.0])))

(defn aifw
  [bn]
  [#{["C1" "true"] ["C3" "true"]} 0.0])

(comment (observe-evidence bn)
         (if (> (get-posterior bn [["C1" "true"]])
                (get-posterior bn [["C2" "true"]]))
           #{["C1" "true"] ["C3" "true"]}
           #{["C2" "true"] ["C3" "true"]}))

(defn metrics
  [mpe result]
  (let [tp (count (filter #(= "true" (second %)) (set/intersection result mpe)))
        fp (count (filter #(= "true" (second %)) (set/difference result mpe)))
        tn (count (filter #(= "false" (second %)) (set/intersection result mpe)))
        fn (count (filter #(= "false" (second %)) (set/difference result mpe)))
        prec (if (= 0 (+ tp fp)) 0.0 (double (/ tp (+ tp fp))))
        fdr (if (= 0 (+ tp fp)) 0.0 (double (/ fp (+ tp fp))))
        accuracy (if (= 0 (+ tp fp tn fn)) 0.0
                     (double (/ (+ tp tn) (+ tp fp tn fn))))
        c1-or-c2 (not-empty (set/intersection #{["C1" "true"] ["C2" true]} result))
        c3 (not-empty (set/intersection #{["C3" "true"]} result))]
    {:mpe-prec prec
     :mpe-fdr fdr
     :mpe-acc accuracy
     :c1-or-c2 c1-or-c2
     :c3 c3}))

(defn avg
  [vals]
  (if (empty? vals) 0.0
      (/ (double (reduce + vals)) (double (count vals)))))

(defn nil-inc
  [v]
  (if (nil? v) 1 (inc v)))

(defn do-experiment
  [iters]
  (dosync (alter state/batch (constantly true)))
  (binding [rgen (new-seed 0)]
    (loop [i 0
           counts {:causal 0 :total 0}]
      (if (= i iters) counts
          (let [{:keys [expgraph bayesnet]} (loop []
                                              (let [net (make-net)]
                                                (if (valid-bayesnet? (:bayesnet net))
                                                  net
                                                  (recur))))
                causal? (causal-bayesnet? bayesnet)]
            (if-not causal?
              (recur i counts)
              (let [abd (aifw bayesnet)
                    mpe (mpe bayesnet)
                    map-gardenfors (bayes-map-gardenfors bayesnet false)
                    map-gardenfors-exp (bayes-map-gardenfors bayesnet true)
                    mre (mre bayesnet)
                    decampos-indep (decampos bayesnet true)
                    decampos-rel (decampos bayesnet false)
                    update-counts (fn [counts key [result _]]
                                    (if causal?
                                      (-> counts
                                         (update-in [false key :matched] #(if (= (first abd) result) (nil-inc %) (or % 0)))
                                         (update-in [false key :metrics] conj (metrics (first mpe) result))
                                         (update-in [false key result] nil-inc)
                                         (update-in [true key :matched] #(if (= (first abd) result) (nil-inc %) (or % 0)))
                                         (update-in [true key :metrics] conj (metrics (first mpe) result))
                                         (update-in [true key result] nil-inc))
                                      (-> counts
                                         (update-in [false key :matched] #(if (= (first abd) result) (nil-inc %) (or % 0)))
                                         (update-in [false key :metrics] conj (metrics (first mpe) result))
                                         (update-in [false key result] nil-inc))))]
                (comment
                  (println "causal?" causal?)
                  (println "abd:" abd)
                  (println "mpe:" mpe)
                  (println "map-gardenfors:" map-gardenfors)
                  (println "mre:" mre)
                  (println "decampos-indep:" decampos-indep)
                  (println "decampos-rel:" decampos-rel)
                  (println expgraph)
                  (println))
                (when (= 0 (mod i 100)) (print (format "%d..." i)) (flush))
                (recur (inc i)
                       (-> (if causal?
                            (-> counts
                               (update-in [:total] inc)
                               (update-in [:causal] inc))
                            (update-in counts [:total] inc))
                          (update-counts :abd abd)
                          (update-counts :mpe mpe)
                          (update-counts :map-gardenfors map-gardenfors)
                          (update-counts :map-gardenfors-exp map-gardenfors-exp)
                          (update-counts :mre mre)
                          (update-counts :decampos-indep decampos-indep)
                          (update-counts :decampos-rel decampos-rel))))))))))

(defn print-results
  [results causal?]
  (let [cnt (get results (if causal? :causal :total))
        r-to-str (fn [r] (str/join ", " (map (fn [[c tf]] (format "%s = %5s" c tf)) (sort-by first r))))]
    (doseq [[k m] (get results causal?)]
      (println (format "%20s --  matches abd: %5d (%6.2f%%) average mpe-acc: %6.2f  c1-or-c2: %6.2f%%  c3: %6.2f%%"
                  (name k) (:matched m)
                  (double (* 100.0 (/ (:matched m) cnt)))
                  (avg (map :mpe-acc (:metrics m)))
                  (double (* 100.0 (/ (count (filter :c1-or-c2 (:metrics m))) cnt)))
                  (double (* 100.0 (/ (count (filter :c3 (:metrics m))) cnt)))))
      (doseq [[r v] (sort-by (comp r-to-str first) (filter (comp set? first) m))]
        (println (format "\t\t%5d (%6.2f%%)            {%s}"
                    v (double (* 100.0 (/ v cnt)))
                    (r-to-str r))))
      (println))))

(defn -main
  []
  (let [results (do-experiment 10000)]
    (println)
    (println)
    (comment
      (println "Total:" (get results :total))
      (println)
      (print-results results false)
      (println))
    (println (format "Causal: %d (%6.2f%%)" (get results :causal)
                (double (* 100.0 (/ (get results :causal)
                                    (get results :total))))))
    (println)
    (print-results results true)
    (println)))
