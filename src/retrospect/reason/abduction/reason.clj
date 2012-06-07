(ns retrospect.reason.abduction.reason
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace
                update-hypotheses init-kb update-kb reset-workspace accepted?
                calc-doubt calc-coverage extract-training lookup-hyp
                inject-accepted-hyps]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [run-scorer get-words]])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate :only [evaluate-helper]])
  (:use [retrospect.reason.abduction.meta
         :only [metareasoning-activated? workspace-compare]])
  (:use [retrospect.reason.abduction.evaluate
         :only [evaluate evaluate-comp update-training
                group-hyps-by-true-false]])
  (:use [retrospect.reason.abduction.gui.hypgraph
         :only [hypgraph-tab update-hypgraph]])
  (:use [retrospect.reason.abduction.gui.logs
         :only [logs-tab update-logs]])
  (:use [retrospect.state]))

(defn find-false-accepted
  [workspace true-false-types]
  (filter #(accepted? workspace %)
     (mapcat (fn [type] (get-in true-false-types [type :all false]))
             (keys true-false-types))))

(defn get-true-false-types
  [workspace truedata time-now]
  (reduce
   (fn [m t]
     (assoc m t
            (group-hyps-by-true-false
             (map #(lookup-hyp workspace %)
                (get (:hypotheses workspace) t))
             :subtype truedata workspace
             time-now (:true-hyp?-fn (:abduction @problem)))))
   {} (keys (dissoc (:hypotheses workspace) :all))))

(defn get-true-false-all
  [true-false-types]
  {true (set (mapcat (fn [type]
                       (mapcat (fn [subtype]
                                 (get-in true-false-types
                                         [type subtype true]))
                               (keys (get true-false-types type))))
                     (keys true-false-types)))
   false (set (mapcat (fn [type]
                        (mapcat (fn [subtype]
                                  (get-in true-false-types
                                          [type subtype false]))
                                (keys (get true-false-types type))))
                      (keys true-false-types)))})

(defn reason-train
  [truedata workspace time-prev time-now sensors]
  (let [ws-orig (if (= "none" (:Oracle params)) workspace
                    (assoc workspace :oracle
                           (partial (:true-hyp?-fn (:abduction @problem))
                                    truedata time-now)))]
    (loop [ws ws-orig
           temp (:StartingTemp params)] ;; "temperature"
      (let [ws-result
            (if sensors
              (explain (update-hypotheses
                        (add-sensor-hyps ws time-prev time-now sensors)))
              (explain (update-hypotheses ws)))
            true-false-types (get-true-false-types ws-result truedata time-now)
            true-false-all (get-true-false-all true-false-types)
            false-accepted (find-false-accepted ws-result true-false-types)
            unexplained (:unexplained (:log ws-result))]
        (when (= "Tracking" (:name @problem))
          (when (or (>= 0.0 temp) (= (:StartingTemp params) temp)
                    (and (= 0 (count false-accepted))
                         (= 0 (count unexplained))))
            (when (and (= 1 time-now) (= (:StartingTemp params) temp))
              (.print System/out "time,pctfalseacc,doubt,maxadjlength,minadjustlength,avgadjustlength,avgmaxadjust,avgminadjust,numadjust,tpr,fpr,f1,begend\n"))
            (let [adjustments (vals (:score-adjustments ws-result))
                  max-adjust-length (if (empty? adjustments) 0 (apply max (map count adjustments)))
                  min-adjust-length (if (empty? adjustments) 0 (apply min (map count adjustments)))
                  avg-adjust-length (/ (double (reduce + (map count adjustments))) (double (count adjustments)))
                  avg-max-adjusted-score (/ (double (reduce + (map #(apply max 0.0 %) adjustments)))
                                            (double (count adjustments)))
                  avg-min-adjusted-score (/ (double (reduce + (map #(apply min 1.0 %) adjustments)))
                                            (double (count adjustments)))
                  {:keys [TPR FPR F1]} (evaluate-helper truedata ws-result time-now)]
              (.print System/out (format "%d,%.4f,%.4f,%d,%d,%.2f,%.2f,%.2f,%d,%.4f,%.4f,%.4f,\"%s\"\n"
                                    time-now
                                    (double (/ (count false-accepted)
                                               (count (:forced ws-result))))
                                    (calc-doubt ws-result)
                                    max-adjust-length min-adjust-length avg-adjust-length
                                    avg-max-adjusted-score avg-min-adjusted-score
                                    (count adjustments) TPR FPR F1
                                    (if (= (:StartingTemp params) temp) "beg" "end"))))))
        (when (= "Words" (:name @problem))
          (when (or (>= 0.0 temp) (= (:StartingTemp params) temp)
                    (and (= 0 (count false-accepted))
                         (= 0 (count unexplained))))
            (when (and (= 1 time-now) (= (:StartingTemp params) temp))
              (.print System/out "time,pctfalseacc,doubt,fscore,ivrecall,oovrecall,oovrate,maxadjlength,minadjustlength,avgadjustlength,avgmaxadjust,avgminadjust,numadjust,begend\n"))
            (let [[prec recall f-score oov-rate oov-recall iv-recall]
                  (run-scorer
                   [(nth (:test-sentences truedata) (dec time-now))]
                   [(get-words
                     (partial lookup-hyp ws-result)
                     (get (:test truedata) (dec time-now))
                     (:accepted ws-result)
                     (:unexplained (:log ws-result)))]
                   (:dict (lookup-hyp ws-result (first (get (:accepted ws-result) :kb)))))
                  adjustments (vals (:score-adjustments ws-result))
                  max-adjust-length (if (empty? adjustments) 0 (apply max (map count adjustments)))
                  min-adjust-length (if (empty? adjustments) 0 (apply min (map count adjustments)))
                  avg-adjust-length (/ (double (reduce + (map count adjustments))) (double (count adjustments)))
                  avg-max-adjusted-score (/ (double (reduce + (map #(apply max 0.0 %) adjustments)))
                                            (double (count adjustments)))
                  avg-min-adjusted-score (/ (double (reduce + (map #(apply min 1.0 %) adjustments)))
                                            (double (count adjustments)))]
              (.print System/out (format "%d,%.4f,%.4f,%.4f,%.4f,%.4f,%.4f,%d,%d,%.2f,%.2f,%.2f,%d,\"%s\"\n"
                                    time-now
                                    (double (/ (count false-accepted)
                                               (count (:forced ws-result))))
                                    (calc-doubt ws-result)
                                    f-score iv-recall oov-recall oov-rate
                                    max-adjust-length min-adjust-length avg-adjust-length
                                    avg-max-adjusted-score avg-min-adjusted-score
                                    (count adjustments)
                                    (if (= (:StartingTemp params) temp) "beg" "end"))))))
        #_(.print System/out (format "%d/%d-" (count false-accepted) (count unexplained)))
        (.flush System/out)
        (cond (not training?)
              (update-kb ws-result)
              (and (= 0 (count false-accepted))
                   (= 0 (count unexplained)))
              (update-kb ws-result)
              (>= 0.0 temp) ;; temperature ran out; ensure only correct stuff is accepted
              (update-kb (inject-accepted-hyps ws-result true-false-all))
              :else
              (let [ws-scored (update-training ws-result true-false-types
                                               true-false-all temp)]
                (recur (assoc ws :scores (:scores ws-scored)
                              :score-adjustments (:score-adjustments ws-scored))
                       (- temp 0.2))))))))

(def reason-abduction
  {:name "Abduction"
   :reason-fn reason-train
   :stats-fn (fn [truedata ors time-now] ((:stats-fn (:abduction @problem))
                                          truedata ors time-now))
   :metareasoning-activated?-fn metareasoning-activated?
   :workspace-compare-fn workspace-compare
   :extract-training-fn extract-training
   :evaluate-fn evaluate
   :evaluate-comp-fn evaluate-comp
   :calc-doubt-fn calc-doubt
   :calc-coverage-fn calc-coverage
   :default-params-fn (fn []
                        (merge {:Threshold [0 [0]]
                                :ConfThreshold [0 [0]]
                                :UseScores [true [true]]
                                :ContrastPreference ["delta" ["delta" "arbitrary"]]
                                :ApplyBoosting [true [true false]]
                                :HypPreference ["abd" ["abd" "arbitrary"]]
                                :TransitiveExplanation [false [true false]]
                                :StartingTemp [1.0 [1.0]]
                                :TempMult [0.05 [0.05]]
                                :ConfAdjustment ["max" ["min" "max" "avg" "none" "norm"]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :reset-workspace-fn reset-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
