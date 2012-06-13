(ns retrospect.reason.abduction.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct lookup-score calc-doubt calc-coverage
                accepted? lookup-score lookup-hyp find-conflicts-all explains]])
  (:use [retrospect.state]))

(defn group-hyps-by-true-false
  [hyps type-key truedata time true-hyp?]
  (let [hs (group-by type-key hyps)
        tf (reduce (fn [m type]
                (let [grouped (group-by (fn [h] (if (true-hyp? truedata time h)
                                                 true false))
                                        (get hs type))
                      m-individual (reduce (fn [m2 [tf hs]]
                                        (reduce (fn [m3 h] (assoc-in m3 [:individual (:id h)] tf))
                                           m2 hs))
                                      m (seq grouped))]
                  (assoc m-individual type
                         (reduce (fn [g tf] (if (nil? (get g tf)) (assoc g tf []) g))
                            grouped [true false]))))
              {} (set (concat (keys hs) (:hyp-types (:abduction @problem)))))
        all-true (mapcat #(get % true) (vals tf))
        all-false (mapcat #(get % false) (vals tf))]
    (assoc tf :all {true all-true false all-false})))

(defn calc-true-false-scores
  "Find average scores for true hyps, average scores for false hyps."
  [workspace true-false]
  (let [scores (reduce (fn [m t]
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
             (keyword (format "AvgTrueScore%s" k))
             (avg (get (get scores t) true))
             (keyword (format "AvgFalseScore%s" k))
             (avg (get (get scores t) false)))))
       {} (keys true-false))))

(defn get-best-true
  [workspace hyps true-false-types]
  (first (reverse (sort-by #(lookup-score workspace %)
                           (filter #(get-in true-false-types [:individual (:id %)]) hyps)))))

(defn update-training
  [workspace true-false-types unexplained]
  (let [bests (reverse (sort-by :delta (:best (:log workspace))))
        biggest-mistake (first (drop-while
                                #(get-in true-false-types [:individual (:id (:best %))])
                                bests))
        wrong-choice (:best biggest-mistake)
        correct-conflicting (when wrong-choice
                              (get-best-true
                               workspace (find-conflicts-all workspace wrong-choice)
                               true-false-types))
        better-choice (or (get-best-true workspace (:alts biggest-mistake) true-false-types)
                          correct-conflicting)
        delta (when better-choice (Math/abs (- (lookup-score workspace wrong-choice)
                                               (lookup-score workspace better-choice))))
        training-adjust (:TrainingAdjustment params)
        adjust (if delta (+ (* 0.50 delta) training-adjust))
        wrong-prior
        (get-in workspace
                [:scores [(:type wrong-choice) (:subtype wrong-choice)]] 0.5)
        better-prior
        (get-in workspace
                [:scores [(:type better-choice) (:subtype better-choice)]] 0.5)]
    ;; don't adjust too much
    (if (and adjust (> adjust (:TrainingMaxAdjust params))) workspace
        (cond better-choice
              (-> workspace
                 (update-in [:score-adjustments
                             [(:type wrong-choice) (:subtype wrong-choice)]]
                            conj (max 0.0 (- wrong-prior adjust)))
                 (assoc-in [:scores
                            [(:type wrong-choice) (:subtype wrong-choice)]]
                           (max 0.0 (- wrong-prior adjust)))
                 (update-in [:score-adjustments
                             [(:type better-choice) (:subtype better-choice)]]
                            conj (min 1.0 (+ better-prior adjust)))
                 (assoc-in [:scores
                            [(:type better-choice) (:subtype better-choice)]]
                           (min 1.0 (+ better-prior adjust))))
              ;; check if this happens any more now that we have merges/splits
              wrong-choice
              (-> workspace
                 (update-in [:score-adjustments
                             [(:type wrong-choice) (:subtype wrong-choice)]]
                            conj (max 0.0 (- wrong-prior training-adjust)))
                 (assoc-in [:scores
                            [(:type wrong-choice) (:subtype wrong-choice)]]
                           (max 0.0 (- wrong-prior training-adjust))))
              ;; no wrong choice, meanining nothing wrong was accepted
              ;; so there must be unexplained data (that had
              ;; explainers); so increase scores on all true hyps,
              ;; decrease scores on all false hyps
              (not-empty unexplained)
              (let [ws-penalized
                    (reduce (fn [ws h]
                         (let [prior (get-in ws [:scores [(:type h) (:subtype h)]] 0.5)]
                           (-> ws
                              (update-in [:score-adjustments [(:type h) (:subtype h)]]
                                         conj (max 0.0 (- prior training-adjust)))
                              (assoc-in [:scores [(:type h) (:subtype h)]]
                                        (max 0.0 (- prior training-adjust))))))
                       workspace (filter #(not= :kb (:type %))
                                    (get-in true-false-types [:all false])))]
                (reduce (fn [ws h]
                     (let [prior (get-in ws [:scores [(:type h) (:subtype h)]] 0.5)]
                       (-> ws
                          (update-in [:score-adjustments [(:type h) (:subtype h)]]
                                     conj (min 1.0 (+ prior training-adjust)))
                          (assoc-in [:scores [(:type h) (:subtype h)]]
                                    (min 1.0 (+ prior training-adjust))))))
                   ws-penalized (filter #(not= :kb (:type %))
                                   (get-in true-false-types [:all true]))))
              :else workspace))))

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        workspace (:workspace ep)
        true-false (group-hyps-by-true-false
                    (filter #(not ((:forced workspace) (:id %)))
                       (vals (:hyp-ids workspace)))
                    :type truedata
                    (:time ep) (:true-hyp?-fn (:abduction @problem)))
        true-false-scores (calc-true-false-scores workspace true-false)]
    (merge {:Problem (:name @problem)}
           params
           ((:evaluate-fn (:abduction @problem)) truedata est)
           true-false-scores
           {:Step (:time ep)
            :UnexplainedPct (get-unexp-pct (:workspace ep))
            :NoExplainersPct (get-noexp-pct (:workspace ep))
            :Doubt (calc-doubt (:workspace ep))
            :Coverage (calc-coverage (:workspace ep))
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
