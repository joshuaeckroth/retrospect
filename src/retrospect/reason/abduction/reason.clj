(ns retrospect.reason.abduction.reason
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace
                update-hypotheses get-explaining-hypotheses
                increase-score decrease-score
                increase-co-occurrence decrease-co-occurrence
                init-kb update-kb reset-workspace accepted?
                calc-doubt calc-coverage extract-training lookup-hyp
                inject-true-hyps find-no-explainers]])
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
  (filter #(accepted? workspace %) (get-in true-false-types [:all false])))

(defn reason-train
  [truedata workspace time-prev time-now sensors]
  (if training?
    (let [ws (add-sensor-hyps workspace time-prev time-now sensors)
          hyps (get-explaining-hypotheses ws)
          true-false-types (group-hyps-by-true-false
                            hyps :type truedata
                            time-now (:true-hyp?-fn (:abduction @problem)))
          occurrences (set (filter identity (map (comp first :co-occurrence)
                                          (get-in true-false-types [:all true]))))
          ws-co-occur (reduce (fn [ws h]
                           (let [occur-id (first (:co-occurrence h))]
                             (if (nil? occur-id) ws
                                 (reduce (fn [ws2 co-occur-id]
                                      (if (occurrences co-occur-id)
                                        (increase-co-occurrence ws2 occur-id co-occur-id)
                                        (decrease-co-occurrence ws2 occur-id co-occur-id)))
                                    ws (second (:co-occurrence h))))))
                         ws (get-in true-false-types [:all true]))
          ws-scored (reduce (fn [ws h] (if (get-in true-false-types [:individual (:id h)])
                                   (increase-score ws h)
                                   (decrease-score ws h)))
                       ws-co-occur hyps)]
      (update-kb (inject-true-hyps ws-scored true-false-types)))
    (if sensors
      (update-kb (explain (update-hypotheses
                           (add-sensor-hyps workspace time-prev time-now sensors))))
      (update-kb (explain (update-hypotheses workspace))))))

(comment
  (defn reason-train
    [truedata workspace time-prev time-now sensors]
    (let [ws-orig (if (= "none" (:Oracle params)) workspace
                      (assoc workspace :oracle
                             (partial (:true-hyp?-fn (:abduction @problem))
                                      truedata time-now)))]
      (loop [ws ws-orig
             cycle (:TrainingCycles params)]
        (let [ws-result
              (if sensors
                (explain (update-hypotheses
                          (add-sensor-hyps ws time-prev time-now sensors)))
                (explain (update-hypotheses ws)))
              true-false-types (group-hyps-by-true-false
                                (vals (:hyp-ids ws-result))
                                :type truedata
                                time-now (:true-hyp?-fn (:abduction @problem)))
              false-accepted (find-false-accepted ws-result true-false-types)
              no-explainers (find-no-explainers ws-result)
              unexplained (set/difference (set (:unexplained (:log ws-result)))
                                          (set no-explainers))]
          (when (:TrainingStats params)
            ((:training-stats-fn (:abduction @problem))
             ws-result false-accepted unexplained truedata time-now cycle))
          (cond (not training?)
                (update-kb ws-result)
                (and (= 0 (count false-accepted))
                     (= 0 (count unexplained)))
                (update-kb ws-result)
                (= 0 cycle) ;; done training; ensure only correct stuff is accepted
                (update-kb (inject-true-hyps ws-result true-false-types))
                :else
                (let [ws-scored (update-training ws-result true-false-types unexplained)]
                  (recur (assoc ws :scores (:scores ws-scored)
                                :score-adjustments (:score-adjustments ws-scored))
                         (dec cycle)))))))))

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
                                :UseScores [true [true]]
                                :ContrastPreference ["delta" ["delta" "arbitrary"]]
                                :HypPreference ["abd" ["abd" "arbitrary"]]
                                :TransitiveExplanation [false [true false]]}
                               (:default-params (:abduction @problem))))
   :init-workspace-fn init-workspace
   :reset-workspace-fn reset-workspace
   :init-kb-fn init-kb
   :player-fns
   {:get-tabs-fn (fn [] [["Logs" (logs-tab)]
                         ["Hypgraph" (hypgraph-tab)]])
    :update-tabs-fn (fn [] (do (update-hypgraph) (update-logs)))}})
