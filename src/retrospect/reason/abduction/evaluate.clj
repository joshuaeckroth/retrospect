(ns retrospect.reason.abduction.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct hyp-conf calc-doubt calc-coverage
                accepted? lookup-score]])
  (:use [retrospect.state]))

(defn group-hyps-by-true-false
  [hyps type-key truedata workspace time true-hyp?]
  (let [hs (group-by type-key (set/difference (set hyps) (:forced workspace)))
        tf (reduce (fn [m subtype]
                     (let [grouped (group-by (fn [h] (if (true-hyp? truedata time h)
                                                       true false))
                                             (get hs subtype))]
                       (assoc m subtype
                              (reduce (fn [g tf] (if (nil? (get g tf)) (assoc g tf []) g))
                                      grouped [true false]))))
                   {} (keys hs))
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
                                 {true (map #(lookup-score workspace %)
                                            (get (get true-false t) true))
                                  false (map #(lookup-score workspace %)
                                             (get (get true-false t) false))}))
                        {} (keys true-false))
        avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + vals) (count vals))))]
    (reduce (fn [m t]
              (let [k (apply str (map str/capitalize (str/split (name t) #"-")))]
                (assoc m
                  (keyword (format "TrueCount%s" k))
                  (count (get (get true-false t) true))
                  (keyword (format "FalseCount%s" k))
                  (count (get (get true-false t) false))
                  (keyword (format "TrueAcc%s" k))
                  (count (filter #(accepted? workspace %) (get (get true-false t) true)))
                  (keyword (format "FalseAcc%s" k))
                  (count (filter #(accepted? workspace %) (get (get true-false t) false)))
                  (keyword (format "AvgTrueConf%s" k))
                  (avg (get (get confs t) true))
                  (keyword (format "AvgTrueApriori%s" k))
                  (avg (get (get apriori t) true))
                  (keyword (format "AvgFalseConf%s" k))
                  (avg (get (get confs t) false))
                  (keyword (format "AvgFalseApriori%s" k))
                  (avg (get (get apriori t) false)))))
            {} (keys true-false))))

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        workspace (:workspace ep)
        accepted (:accepted workspace)
        rejected (:rejected workspace)
        true-false (group-hyps-by-true-false
                    (apply concat (vals (:hypotheses workspace))) :type
                    truedata workspace
                    (:time ep) (:true-hyp?-fn (:abduction @problem)))
        true-false-confs (calc-true-false-confs workspace true-false)]
    (merge {:Problem (:name @problem)}
           params
           ((:evaluate-fn (:abduction @problem)) truedata est)
           true-false-confs
           {:Step (:time ep)
            :UnexplainedPct (get-unexp-pct (:workspace ep))
            :NoExplainersPct (get-noexp-pct (:workspace ep))
            :Doubt (calc-doubt (:workspace ep))
            :Coverage (calc-coverage (:workspace ep))
            :ExplainCycles (:cycle workspace)
            :HypothesisCount (reduce + (map count (vals (:hypotheses workspace))))})))

(defn update-training
  [workspace truedata time-now]
  (let [true-false-types (map (fn [t]
                                (group-hyps-by-true-false
                                 (get (:hypotheses workspace) t) :subtype
                                 truedata workspace
                                 time-now (:true-hyp?-fn (:abduction @problem))))
                              (keys (:hypotheses workspace)))]
    (reduce
     (fn [ws tfs] ;; true-false groups (keyed by subtype)
       (reduce
        (fn [ws2 st] ;; subtype key
          (reduce
           (fn [ws3 tf] ;; true/false key
             (reduce
              (fn [ws4 hyp] ;; hyps in that true/false, subtype, type
                (let [prior (get-in ws4 [:scores (:type hyp) (:subtype hyp)] 0.5)]
                  (if tf
                    (assoc-in ws4 [:scores (:type hyp) (:subtype hyp)]
                              (min 1.0 (+ prior 0.05)))
                    (assoc-in ws4 [:scores (:type hyp) (:subtype hyp)]
                              (max 0.0 (- prior 0.05))))))
              ws3 (get-in tfs [st tf])))
           ws2 (keys (get tfs st))))
        ws (keys tfs)))
     workspace true-false-types)))

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
                        (concat [:UnexplainedPct :NoExplainersPct
                                 :Doubt :Coverage :ExplainCycles :HypothesisCount]
                                (mapcat
                                 (fn [tf]
                                   (map #(keyword
                                          (format "%s%s" tf
                                                  (apply str
                                                         (map str/capitalize
                                                              (str/split (name %) #"-")))))
                                        (:hyp-subtypes @problem)))
                                 ["AvgTrueConf" "AvgTrueApriori"
                                  "AvgFalseConf" "AvgFalseApriori"
                                  "TrueCount" "FalseCount" "TrueAcc" "FalseAcc"])))))]
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
