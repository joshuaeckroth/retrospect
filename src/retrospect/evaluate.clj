(ns retrospect.evaluate
  (:require [clojure.set :as set])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [current-ep-state previous-ep-state]])
  (:use [retrospect.workspaces :only [get-unexplained-pct hyp-conf get-hyps]])
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
  (let [hyps (set/difference (get-hyps workspace :static)
                             (:forced workspace))
        true-false (group-by (partial true-hyp? truedata pdata time) hyps)
        true-confs (map #(hyp-conf workspace %) (get true-false true))
        false-confs (map #(hyp-conf workspace %) (get true-false false))
        avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + 0.0 vals) (count vals))))]
    {:AvgTrueConfs (avg true-confs) :AvgFalseConfs (avg false-confs)}))

(defn evaluate
  [truedata or-state]
  (let [ep-state (current-ep-state (:ep-state-tree or-state))
        prev-ep (previous-ep-state (:ep-state-tree or-state))
        final-log (:final (:log (:workspace prev-ep)))
        ors-resources (:resources or-state)
        ws-resources (:resources (:workspace prev-ep))]
    (update-in
     or-state [:results] conj
     (merge {:Problem (:name @problem)}
            params
            ((:evaluate-fn @problem) ep-state (:sensors or-state) truedata)
            (calc-true-false-confs truedata (:pdata ep-state) (:workspace prev-ep)
                                   (:time ep-state) (:true-hyp?-fn @problem))
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
                        [:MetaActivations :MetaAccepted :Milliseconds :SharedExplains
                         :Unexplained :UnexplainedPct :NoExplainers
                         :ExplainCycles :HypothesisCount
                         :Compute :Memory :DeepestDep])))]
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
