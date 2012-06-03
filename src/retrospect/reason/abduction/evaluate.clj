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
  (let [hs (group-by type-key (filter #(not ((:forced workspace) (:id %))) hyps))
        tf (reduce (fn [m subtype]
                     (let [grouped (group-by (fn [h] (if (true-hyp? truedata time h)
                                                       true false))
                                             (get hs subtype))]
                       (assoc m subtype
                              (reduce (fn [g tf] (if (nil? (get g tf)) (assoc g tf []) g))
                                      grouped [true false]))))
                   {} (keys hs))
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

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        workspace (:workspace ep)
        true-false (group-hyps-by-true-false
                    (map #(lookup-hyp workspace %)
                         (apply concat (vals (:hypotheses workspace))))
                    :type truedata workspace
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

(comment
  (cond
   (and (not tf) (accepted? workspace hyp)) ;; false but accepted
   (let [better-choices (filter (get true-false-all true)
                                (find-conflicts-all workspace hyp))
         best-choice (last (sort-by (comp count :explains) better-choices))
         prior-wrong (get-in ws4 [:scores (:type hyp) (:subtype hyp)] 0.5)
         prior-best (get-in ws4 [:scores (:type best-choice)
                                 (:subtype best-choice)] 0.5)]
     (if best-choice
       (-> ws4
           (assoc-in
            [:scores (:type hyp) (:subtype hyp)]
            (max 0.0 (- prior-wrong (* (:TempMult params) temp))))
           (assoc-in
            [:scores (:type best-choice) (:subtype best-choice)]
            (min 1.0 (+ prior-best (* (:TempMult params) temp)))))
       (assoc-in ws4 [:scores (:type hyp) (:subtype hyp)]
                 (max 0.0 (- prior-wrong (* (:TempMult params) temp))))))
   ;; true but not accepted and what it explains is unexplained
   (and tf (some unexplained (explains hyp)))
   (let [prior (get-in ws4 [:scores (:type hyp) (:subtype hyp)] 0.5)]
     (assoc-in ws4 [:scores (:type hyp) (:subtype hyp)]
               (min 1.0 (+ prior (* (:TempMult params) temp)))))
   :else ws4))

(comment (let [unexplained (set (:unexplained (:log workspace)))]
           (reduce
            (fn [ws type] ;; true-false groups (keyed by subtype)
              (reduce
               (fn [ws2 st] ;; subtype key
                 (reduce
                  (fn [ws3 tf] ;; true/false key
                    (reduce
                     (fn [ws4 hyp] ;; hyps in that true/false, subtype, type
                       (let [prior (get-in ws4 [:scores (:type hyp) (:subtype hyp)] 0.5)]
                         (if tf
                           (assoc-in ws4 [:scores (:type hyp) (:subtype hyp)]
                                     (min 1.0 (+ prior (* (:TempMult params) temp))))
                           (assoc-in ws4 [:scores (:type hyp) (:subtype hyp)]
                                     (max 0.0 (- prior (* (:TempMult params) temp)))))))
                     ws3 (get-in true-false-types [type st tf])))
                  ws2 (keys (get-in true-false-types [type st]))))
               ws (keys (get true-false-types type))))
            workspace (keys true-false-types))))

(defn update-training
  [workspace true-false-types true-false-all temp]
  (let [bests (reverse (sort-by :delta (:best (:log workspace))))
        biggest-mistake (first (drop-while #((get true-false-all true) (:best %)) bests))
        wrong-choice (:best biggest-mistake)
        better-choice (first (reverse (sort-by #(hyp-conf workspace %)
                                               (filter (get true-false-all true)
                                                       (:alts biggest-mistake)))))
        delta (:delta biggest-mistake)
        adjust (if delta (+ (* 0.50 delta) 0.01))
        wrong-prior (get-in workspace
                            [:scores (:type wrong-choice) (:subtype wrong-choice)]
                            0.5)
        better-prior (get-in workspace
                             [:scores (:type better-choice) (:subtype better-choice)]
                             0.5)]
    (comment
      (println "biggest mistake" biggest-mistake)
      (println "wrong choice" wrong-choice (hyp-conf workspace wrong-choice)
               (:pos-seq wrong-choice))
      (println "better choice" better-choice (hyp-conf workspace better-choice)
               (:pos-seq better-choice))
      (println "delta" delta "adjust" adjust))
    (if better-choice
      (-> workspace
          (assoc-in [:scores (:type wrong-choice) (:subtype wrong-choice)]
                    (max 0.0 (- wrong-prior adjust)))
          (assoc-in [:scores (:type better-choice) (:subtype better-choice)]
                    (min 1.0 (+ better-prior adjust))))
      (assoc-in workspace [:scores (:type wrong-choice) (:subtype wrong-choice)]
                (max 0.0 (- wrong-prior 0.1))))))

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
