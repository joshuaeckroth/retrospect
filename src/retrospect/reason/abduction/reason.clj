(ns retrospect.reason.abduction.reason
  (:require [clojure.string :as str])
  (:use [retrospect.reason.abduction.workspace
         :only [explain add-sensor-hyps init-workspace
                update-hypotheses init-kb update-kb reset-workspace
                calc-doubt calc-coverage extract-training lookup-hyp]])
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
            true-false-types (reduce
                              (fn [m t]
                                (assoc m t
                                       (group-hyps-by-true-false
                                        (map #(lookup-hyp ws-result %)
                                             (get (:hypotheses ws-result) t))
                                        :subtype truedata ws-result
                                        time-now (:true-hyp?-fn (:abduction @problem)))))
                              {} (keys (dissoc (:hypotheses ws-result) :all)))
            true-false-all {true (set (mapcat (fn [type]
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
                                              (keys true-false-types)))}
            false-accepted (filter #(some #{(:id %)}
                                          (get-in ws-result [:accepted (:type %)]))
                                   (mapcat (fn [tfs] (get (:all tfs) false))
                                           true-false-types))]
        (cond (not training?)
              (update-kb ws-result)
              (= 0 (count false-accepted))
              (update-kb ws-result)
              (>= 0.0 temp) ;; temperature ran out; ensure only correct stuff is accepted
              (let [true-by-type (group-by :type (mapcat (fn [tfs] (get (:all tfs) true))
                                                         true-false-types))]
                (update-kb
                 (reduce (fn [ws t] (assoc-in ws [:accepted t]
                                              (map :id (get true-by-type t))))
                         (assoc-in ws-result [:accepted :all]
                                   (map :id (apply concat (vals true-by-type))))
                         (keys true-by-type))))
              :else
              (recur (assoc ws :scores
                            (:scores (update-training ws-result true-false-types
                                                      true-false-all temp)))
                     (- temp 0.2)))))))

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
