(ns retrospect.reason.abduction.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct hyp-conf get-hyps]])
  #_(:use [retrospect.reason.abduction.robustness
         :only [analyze-sensitivity analyze-dependency-quick]])
  (:use [retrospect.state]))

(defn calc-increase
  [control-results comparison-results field]
  (let [increase-field (keyword (format "Diff%s" (name field)))
        increase-val (- (or (comparison-results field) 0.0)
                        (or (control-results field) 0.0))]
    {increase-field increase-val}))

(comment
 (defn calc-deepest-dep
   [depgraph]
   (let [starts (filter #(empty? (incoming depgraph %)) (nodes depgraph))
         paths (mapcat #(dijkstra-span (partial neighbors depgraph)
                                       (constantly 1) %) starts)]
     (apply max 0 (mapcat (comp vals second) paths))))

 (defn group-hyps-by-subtype-true-false
   [truedata workspace time true-hyp?]
   (let [hyps (reduce (fn [m t] (if (nil? (get m t)) (assoc m t []) m))
                      (group-by :subtype (set/difference (get-hyps workspace :static)
                                                         (:forced workspace)))
                      (:hyp-subtypes @problem))
         tf (reduce (fn [m subtype]
                      (let [grouped (group-by (fn [h] (if (true-hyp? truedata time h)
                                                        true false))
                                              (get hyps subtype))]
                        (assoc m subtype
                               (reduce (fn [g tf] (if (nil? (get g tf)) (assoc g tf []) g))
                                       grouped [true false]))))
                    {} (keys hyps))
         all-true (mapcat #(get % true) (vals tf))
         all-false (mapcat #(get % false) (vals tf))]
     (assoc tf :all {true all-true false all-false})))

 (defn calc-true-false-confs
   "Find average confidence and apriori values for true hyps, average
   confidence for false hyps."
   [workspace true-false]
   (let [confs (reduce (fn [m t]
                         (assoc m t
                                {true (map #(hyp-conf workspace %)
                                           (get (get true-false t) true))
                                 false (map #(hyp-conf workspace %)
                                            (get (get true-false t) false))}))
                       {} (keys true-false))
         apriori (reduce (fn [m t]
                           (assoc m t
                                  {true (map :apriori (get (get true-false t) true))
                                   false (map :apriori (get (get true-false t) false))}))
                         {} (keys true-false))
         avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + vals) (count vals))))]
     (reduce (fn [m t]
               (let [k (apply str (map str/capitalize (str/split (name t) #"-")))]
                 (assoc m
                   (keyword (format "AvgTrueConf%s" k))
                   (avg (get (get confs t) true))
                   (keyword (format "AvgTrueApriori%s" k))
                   (avg (get (get apriori t) true))
                   (keyword (format "AvgFalseConf%s" k))
                   (avg (get (get confs t) false))
                   (keyword (format "AvgFalseApriori%s" k))
                   (avg (get (get apriori t) false)))))
             {} (keys true-false)))))

(comment
  (defn calc-avg-true-false-deps
    [or-state true-false]
    (let [depgraph (:depgraph (:ep-state or-state))
          starts (apply concat (mapcat vals (vals true-false)))
          tf-counts (map (fn [[subtype tf]]
                           (let [hyps (get (get true-false subtype) tf)
                                 deps (set (map first
                                                (mapcat #(analyze-dependency-quick
                                                          or-state % starts)
                                                        hyps)))]
                             [subtype tf (if (empty? hyps) 0.0
                                             (double (/ (count deps) (count hyps))))]))
                         (mapcat (fn [subtype] (map (fn [tf] [subtype tf])
                                                    (keys (get true-false subtype))))
                                 (keys true-false)))]
      (reduce (fn [m [subtype tf avg]]
                (let [k (keyword (format "Avg%sDeps%s" (str/capitalize (str tf))
                                         (apply str (map str/capitalize
                                                         (str/split (name subtype) #"-")))))]
                  (assoc m k avg))) {} tf-counts))))

(comment         true-false (group-hyps-by-subtype-true-false
                    truedata workspace
                    (:time ep) (:true-hyp?-fn @problem)))

(defn evaluate
  [truedata ors]
  (let [ep (cur-ep (:est ors))
        workspace (:workspace ep)
        accepted (:accepted workspace)
        rejected (:rejected workspace)
        final-log (:final (:log workspace))
        ors-resources (:resources ors)
        ws-resources (:resources workspace)]
    (update-in
     ors [:results] conj
     (merge {:Problem (:name @problem)}
            params
            ((:evaluate-fn (:abduction @problem)) accepted rejected (:time ep)
             (:sensors ors) truedata)
            #_(calc-true-false-confs workspace true-false)
            #_(if (:AnalyzeSensitivity params)
              (analyze-sensitivity ors true-false)
              (reduce
               (fn [m tf]
                 (reduce (fn [m2 k] (assoc m2 k 0))
                         m (map #(keyword
                                  (format "Avg%s%s" tf
                                          (apply str
                                                 (map str/capitalize
                                                      (str/split (name %) #"-")))))
                                (:hyp-subtypes (:abduction @problem)))))
               {} ["TrueSensitivity" "FalseSensitivity"]))
            #_(if (:AnalyzeDeps params)
              (calc-avg-true-false-deps or-state true-false)
              (reduce
               (fn [m tf]
                 (reduce (fn [m2 k] (assoc m2 k 0))
                         m (map #(keyword
                                  (format "Avg%s%s" tf
                                          (apply str
                                                 (map str/capitalize
                                                      (str/split (name %) #"-")))))
                                (:hyp-subtypes @problem))))
               {} ["TrueDeps" "FalseDeps"]))
            {:Step (:time ep)
             :MetaActivations (:meta-activations ors-resources)
             :MetaAccepted (:meta-accepted ors-resources)
             :Milliseconds (:milliseconds ors-resources) 
             :UnexplainedPct (get-unexp-pct (:workspace ep))
             :NoExplainersPct (get-noexp-pct (:workspace ep))
             :ExplainCycles (:explain-cycles ws-resources)
             :HypothesisCount (:hypothesis-count ws-resources)}))))

(comment              :DeepestDep (if-not (:AnalyzeDeps params) 0
                                 (calc-deepest-dep (:depgraph ep))))

(defn prefix-params
  [prefix params]
  (zipmap (map (fn [k] (keyword (format "%s%s" prefix (name k)))) (keys params))
          (vals params)))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (letfn [(do-eval [control comparison]
            (apply merge
                   {:Problem (:name @problem)}
                   (prefix-params "Cont" (dissoc control-params :simulation))
                   (prefix-params "Comp" (dissoc comparison-params :simulation))
                   {:simulation (:simulation control-params)
                    :Step (:Step control)}
                   ((:evaluate-comp-fn (:abduction @problem)) control comparison
                    control-params comparison-params)
                   (map #(calc-increase control comparison %)
                        (concat [:MetaActivations :MetaAccepted :Milliseconds
                                 :UnexplainedPct :NoExplainersPct
                                 :ExplainCycles :HypothesisCount :DeepestDep]
                                (mapcat
                                 (fn [tf]
                                   (map #(keyword
                                          (format "Avg%s%s" tf
                                                  (apply str
                                                         (map str/capitalize
                                                              (str/split (name %) #"-")))))
                                        (:hyp-subtypes @problem)))
                                 ["TrueConf" "TrueApriori"
                                  "FalseConf" "FalseApriori"
                                  "TrueDeps" "FalseDeps"
                                  "TrueSensitivity" "FalseSensitivity"])))))]
    ;; if control/comparison have different number of results
    ;; (different steps between or steps), then just use the last
    ;; result set
    (if (not= (count control-results) (count comparison-results))
      (let [control (last control-results)
            comparison (last comparison-results)]
        [(do-eval control comparison)])
      ;; otherwise, evaluate each result set in sequence
      (for [i (range (count control-results))]
        (let [control (nth control-results i)
              comparison (nth comparison-results i)]
          (do-eval control comparison))))))
