(ns retrospect.reason.abduction.evaluate
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [loom.graph :only [incoming nodes neighbors]])
  (:use [loom.alg-generic :only [dijkstra-span]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.reason.abduction.workspace
         :only [get-unexp-pct get-noexp-pct hyp-conf calc-doubt calc-coverage
                accepted? lookup-score lookup-hyp find-conflicts-all explains]])
  (:use [retrospect.state]))

(defn group-hyps-by-true-false
  [hyps type-key truedata workspace time true-hyp?]
  (let [hs (group-by type-key hyps)
        tf (reduce (fn [m type]
                (let [grouped (group-by (fn [h] (if (true-hyp? truedata time h)
                                                 true false))
                                        (get hs type))]
                  (assoc m type
                         (reduce (fn [g tf] (if (nil? (get g tf)) (assoc g tf []) g))
                            grouped [true false]))))
              {} (set (concat (keys hs) (:hyp-types (:abduction @problem)))))
        all-true (set (mapcat #(get % true) (vals tf)))
        all-false (set (mapcat #(get % false) (vals tf)))]
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

(defn get-best-true
  [workspace hyps true-false-types]
  (first (reverse (sort-by #(hyp-conf workspace %)
                           (filter (get-in true-false-types [:all true]) hyps)))))

(defn update-training
  [workspace true-false-types unexplained]
  (let [bests (reverse (sort-by :delta (:best (:log workspace))))
        biggest-mistake (first (drop-while #((get-in true-false-types [:all true])
                                             (:best %)) bests))
        wrong-choice (:best biggest-mistake)
        correct-conflicting (when wrong-choice
                              (get-best-true
                               workspace (find-conflicts-all workspace wrong-choice)
                               true-false-types))
        better-choice (or (get-best-true workspace (:alts biggest-mistake) true-false-types)
                          correct-conflicting)
        delta (when better-choice (Math/abs (- (hyp-conf workspace wrong-choice)
                                               (hyp-conf workspace better-choice))))
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
                    :type truedata workspace
                    (:time ep) (:true-hyp?-fn (:abduction @problem)))
        true-false-confs (calc-true-false-confs workspace true-false)
        adjustment-metrics
        (if (not= (:Steps params) (:time ep))
          {:NumAdjustments 0
           :MaxAdjustLength 0
           :MinAdjustLength 0
           :AvgAdjustLength 0.0
           :AvgMaxAdjustedScore 0.0
           :AvgMinAdjustedScore 0.0
           :MaxAdjustedScore 0.0
           :MinAdjustedScore 0.0}
          (let [adjustments (vals (:score-adjustments workspace))
                max-adjust-length (if (empty? adjustments) 0
                                      (apply max (map count adjustments)))
                min-adjust-length (if (empty? adjustments) 0
                                      (apply min (map count adjustments)))
                avg-adjust-length (/ (double (reduce + (map count adjustments)))
                                     (double (count adjustments)))
                avg-max-adjusted-score (/ (double (reduce + (map #(apply max 0.0 %) adjustments)))
                                          (double (count adjustments)))
                avg-min-adjusted-score (/ (double (reduce + (map #(apply min 1.0 %) adjustments)))
                                          (double (count adjustments)))
                max-adjusted-score (apply max 0.0 (apply concat adjustments))
                min-adjusted-score (apply min 1.0 (apply concat adjustments))]
            {:NumAdjustments (count adjustments)
             :MaxAdjustLength max-adjust-length
             :MinAdjustLength min-adjust-length
             :AvgAdjustLength avg-adjust-length
             :AvgMaxAdjustedScore avg-max-adjusted-score
             :AvgMinAdjustedScore avg-min-adjusted-score
             :MaxAdjustedScore max-adjusted-score
             :MinAdjustedScore min-adjusted-score}))]
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
            :HypothesisCount (reduce + (map count (vals (:hypotheses workspace))))}
           adjustment-metrics)))

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
