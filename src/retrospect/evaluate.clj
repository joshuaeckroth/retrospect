(ns retrospect.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [current-ep-state previous-ep-state]])
  (:use [retrospect.workspaces :only [get-unexplained-pct hyp-conf get-hyps]])
  (:use [retrospect.meta.robustness :only [analyze-sensitivity analyze-dependency]])
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

(defn calc-true-false-confs
  "Find average confidence for true hyps, average confidence for false hyps."
  [truedata pdata workspace time true-hyp?]
  (let [hyps (reduce (fn [m t] (if (nil? (get m t)) (assoc m t #{}) m))
                     (group-by :subtype (set/difference (get-hyps workspace :static)
                                                        (:forced workspace)))
                     (:hyp-subtypes @problem))
        true-false (let [tf (reduce (fn [m t]
                                      (assoc m t (group-by
                                                  (partial true-hyp? truedata pdata time)
                                                  (get hyps t))))
                                    {} (keys hyps))
                         all-true (mapcat #(get % true) (vals tf))
                         all-false (mapcat #(get % false) (vals tf))]
                     (assoc tf :all {true all-true false all-false}))
        confs (reduce (fn [m t]
                        (assoc m t
                               {true (map #(hyp-conf workspace %)
                                          (get (get true-false t) true))
                                false (map #(hyp-conf workspace %)
                                           (get (get true-false t) false))}))
                      {} (keys true-false))
        avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + vals) (count vals))))]
    (reduce (fn [m t]
              (let [k (apply str (map str/capitalize (str/split (name t) #"-")))]
                (assoc m (keyword (format "AvgTrue%s" k)) (avg (get (get confs t) true))
                       (keyword (format "AvgFalse%s" k)) (avg (get (get confs t) false)))))
            {} (keys confs))))

(defn calc-avg-true-false-deps
  [truedata or-state ep-state pdata workspace time true-hyp?]
  (let [depgraph (:depgraph ep-state)
        starts (filter #(empty? (incoming depgraph %)) (nodes depgraph))
        tf-starts (group-by (partial true-hyp? truedata pdata time) starts)
        tf-counts (map (fn [tf] (let [hyps (get tf-starts tf)
                                      deps (set (map first
                                                     (mapcat #(analyze-dependency or-state %)
                                                             hyps)))]
                                  (count deps)))
                       [true false])]
    {:AvgTrueDeps (if (empty? (get tf-starts true)) 0.0
                      (double (/ (first tf-counts) (count (get tf-starts true)))))
     :AvgFalseDeps (if (empty? (get tf-starts false)) 0.0
                       (double (/ (second tf-counts) (count (get tf-starts false)))))}))

(defn evaluate
  [truedata or-state]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        prev-ep (previous-ep-state (:ep-state-tree or-state))
        workspace (:workspace prev-ep)
        final-log (:final (:log workspace))
        ors-resources (:resources or-state)
        ws-resources (:resources workspace)]
    (update-in
     or-state [:results] conj
     (merge {:Problem (:name @problem)}
            params
            ((:evaluate-fn @problem) ep-state (:sensors or-state) truedata)
            (calc-true-false-confs truedata (:pdata ep-state) (:workspace prev-ep)
                                   (:time ep-state) (:true-hyp?-fn @problem))
            (if (:AnalyzeSensitivity params)
              (analyze-sensitivity or-state truedata)
              {:AvgTrueSensitivity 0.0 :AvgFalseSensitivity 0.0
               :CountTrueSame 0 :CountFalseSame 0})
            (calc-avg-true-false-deps truedata or-state ep-state (:pdata ep-state)
                                      (:workspace prev-ep) (:time ep-state)
                                      (:true-hyp?-fn @problem))
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
                                 ["True" "False"])))))]
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
