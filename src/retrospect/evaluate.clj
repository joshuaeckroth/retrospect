(ns retrospect.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [current-ep-state previous-ep-state]])
  (:use [retrospect.workspaces :only [get-unexplained-pct hyp-conf get-hyps]])
  (:use [retrospect.meta.robustness :only [analyze-sensitivity analyze-dependency-quick]])
  (:use [retrospect.state]))

(defn calc-increase
  [control-results comparison-results field]
  (let [increase-field (keyword (format "Inc%s" (name field)))
        increase-val (if (= 0 (control-results field)) 0.0
                         (double (* 100.0 (/ (- (comparison-results field)
                                                (control-results field))
                                             (control-results field)))))]
    {increase-field increase-val}))

(defn calc-deepest-dep
  [depgraph]
  (let [starts (filter #(empty? (incoming depgraph %)) (nodes depgraph))
        paths (mapcat #(dijkstra-span (partial neighbors depgraph)
                                      (constantly 1) %) starts)]
    (apply max 0 (mapcat (comp vals second) paths))))

(defn group-hyps-by-subtype-true-false
  [truedata pdata workspace time true-hyp?]
  (let [hyps (reduce (fn [m t] (if (nil? (get m t)) (assoc m t []) m))
                     (group-by :subtype (set/difference (get-hyps workspace :static)
                                                        (:forced workspace)))
                     (:hyp-subtypes @problem))
        tf (reduce (fn [m subtype]
                     (let [grouped (group-by (partial true-hyp? truedata pdata time)
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
            {} (keys true-false))))

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
                (assoc m k avg))) {} tf-counts)))

(defn evaluate
  [truedata or-state]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        prev-ep (previous-ep-state (:ep-state-tree or-state))
        workspace (:workspace prev-ep)
        final-log (:final (:log workspace))
        ors-resources (:resources or-state)
        ws-resources (:resources workspace)
        true-false (group-hyps-by-subtype-true-false
                    truedata (:problem-data ep-state) (:workspace prev-ep)
                    (:time ep-state) (:true-hyp?-fn @problem))]
    (update-in
     or-state [:results] conj
     (merge {:Problem (:name @problem)}
            params
            ((:evaluate-fn @problem) ep-state (:sensors or-state) truedata)
            (calc-true-false-confs workspace true-false)
            (if (:AnalyzeSensitivity params)
              (analyze-sensitivity or-state truedata)
              {:AvgTrueSensitivity 0.0 :AvgFalseSensitivity 0.0
               :CountTrueSame 0 :CountFalseSame 0})
            (calc-avg-true-false-deps or-state true-false)
            {:Step (:time ep-state)
             :MetaActivations (:meta-activations ors-resources)
             :MetaAccepted (:meta-accepted ors-resources)
             :Milliseconds (:milliseconds ors-resources) 
             :Unexplained (count (:unexplained final-log))
             :UnexplainedPct (* 100.0 (get-unexplained-pct (:workspace prev-ep)))
             :NoExplainers (count (:no-explainers final-log))
             :SharedExplains (count (:shared-explains final-log))
             :ExplainCycles (:explain-cycles ws-resources)
             :HypothesisCount (:hypothesis-count ws-resources)
             :Compute (:compute ors-resources)
             :Memory (:memory ors-resources)
             :DeepestDep (calc-deepest-dep (:depgraph ep-state))}))))

(defn prefix-params
  [prefix params]
  (zipmap (map (fn [k] (keyword (format "%s%s" prefix (name k)))) (keys params))
          (vals params)))

(defn evaluate-comparative
  [control-results comparison-results control-params comparison-params]
  (letfn [(do-eval [control comparison]
            (apply merge
                   {:Problem (:name @problem)}
                   (prefix-params "Cont" (dissoc control-params :simulation))
                   (prefix-params "Comp" (dissoc comparison-params :simulation))
                   {:simulation (:simulation control-params)
                    :Step (:Step control)}
                   ((:evaluate-comparative-fn @problem) control comparison
                    control-params comparison-params)
                   (map #(calc-increase control comparison %)
                        (concat [:MetaActivations :MetaAccepted :Milliseconds :SharedExplains
                                 :Unexplained :UnexplainedPct :NoExplainers
                                 :ExplainCycles :HypothesisCount
                                 :Compute :Memory :DeepestDep
                                 :AvgTrueSensitivity :AvgFalseSensitivity
                                 :CountTrueSame :CountFalseSame
                                 :AvgTrueDeps :AvgFalseDeps]
                                (mapcat
                                 (fn [tf]
                                   (map #(keyword
                                          (format "Avg%s%s" tf
                                                  (apply str
                                                         (map str/capitalize
                                                              (str/split (name %) #"-")))))
                                        (:hyp-subtypes @problem)))
                                 ["TrueConf" "TrueApriori"
                                  "FalseConf" "FalseApriori"])))))]
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
