(ns retrospect.problems.tracking.problem
  (:require [retrospect problem])
  (:import [retrospect.problem Problem])
  (:use [retrospect.problems.tracking.evaluate :only [evaluate]])
  (:use [retrospect.problems.tracking.truedata :only [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only [generate-sensors]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [hypothesize commit-decision]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-params player-set-params player-get-params-panel
          player-get-diagram player-get-stats-panel
          player-update-stats player-update-truedata-log-box
          player-update-problem-log-box]])
  (:use [retrospect.problems.tracking.sensors :only
         [measure-sensor-overlap measure-sensor-coverage
          list-sensors-seen list-sensors-unseen sensors-seen-grid]])
  (:use [retrospect.problems.tracking.monitor :only [monitor]])
  (:use [retrospect.problems.tracking.prepared :only [prepared-map]]))

(defn generate-problem-data
  [sensors params]
  {:paths {}
   :sensors-seen-grid (sensors-seen-grid sensors params)
   :spotted-grid []
   :sensors-seen
   (list-sensors-seen (:GridWidth params) (:GridHeight params) sensors)
   :sensors-unseen
   (list-sensors-unseen (:GridWidth params) (:GridHeight params) sensors)
   :sensor-coverage
   (measure-sensor-coverage (:GridWidth params) (:GridHeight params) sensors)
   :sensor-overlap
   (measure-sensor-overlap (:GridWidth params) (:GridHeight params) sensors)})

(def tracking-problem
  (Problem. "tracking"
            monitor
            {:get-params-fn player-get-params
             :set-params-fn player-set-params
             :get-params-panel-fn player-get-params-panel
             :get-diagram-fn player-get-diagram
             :get-stats-panel-fn player-get-stats-panel
             :update-stats-fn player-update-stats
             :update-truedata-log-box-fn player-update-truedata-log-box
             :update-problem-log-box-fn player-update-problem-log-box}
            generate-truedata
            generate-sensors
            prepared-map
            hypothesize
            commit-decision
            generate-problem-data
            evaluate))
