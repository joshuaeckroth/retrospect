(ns retrospect.reason.abduction.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct hyp-conf]])
  (:use [retrospect.state]))

(defn group-hyps-by-subtype-true-false
  [truedata workspace time true-hyp?]
  (let [hyps (reduce (fn [m t] (if (nil? (get m t)) (assoc m t []) m))
                     (group-by :subtype (set/difference
                                         (set (apply concat (vals (:hypotheses workspace))))
                                         (:forced workspace)))
                     (:hyp-types (:abduction @problem)))
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
        avg (fn [vals] (if (empty? vals) 0.0 (/ (reduce + vals) (count vals))))
        accepted? (fn [hyp] ((set (get (:accepted workspace) (:type hyp))) hyp))]
    (reduce (fn [m t]
              (let [k (apply str (map str/capitalize (str/split (name t) #"-")))]
                (assoc m
                  (keyword (format "TrueCount%s" k))
                  (count (get (get true-false t) true))
                  (keyword (format "FalseCount%s" k))
                  (count (get (get true-false t) false))
                  (keyword (format "TrueAcc%s" k))
                  (count (filter accepted? (get (get true-false t) true)))
                  (keyword (format "FalseAcc%s" k))
                  (count (filter accepted? (get (get true-false t) false)))
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
        true-false (group-hyps-by-subtype-true-false
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
            :Doubt (:doubt (:workspace ep))
            :Coverage (:coverage (:workspace ep))
            :ExplainCycles (:cycle workspace)
            :HypothesisCount (reduce + (map count (vals (:hypotheses workspace))))})))

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
