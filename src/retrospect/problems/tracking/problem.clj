(ns retrospect.problems.tracking.problem
  (:require [retrospect problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str])
  (:use [retrospect.problems.tracking.evaluate :only
         [evaluate evaluate-comparative]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [generate-sensors]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [hypothesize get-more-hyps commit-decision inconsistent]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.tracking.sensors :only
         [sensors-seen-grid]])
  (:use [retrospect.problems.tracking.monitor :only
         [monitor]])
  (:use [retrospect.problems.tracking.prepared :only
         [prepared-map]])
  (:use [retrospect.state]))

(defn read-walk-dist
  [file]
  (let [lines (str/split-lines (slurp file))
        walk-count (Integer/parseInt (first lines))]
    (with-meta (reduce #(assoc %1 (Double/parseDouble (first %2))
                               (Integer/parseInt (second %2)))
                       {} (map #(str/split % #",") (rest lines)))
               {:walk-count walk-count})))

(defn generate-problem-data
  [sensors params]
  {:paths (sorted-map)
   :disbelieved-moves []
   :walk-dist (read-walk-dist (str @datadir "/tracking/walks.txt")) 
   :split-merge-hyps []
   :log [] ;; log is reset each time by commit-decision
   :sensors-seen-grid (sensors-seen-grid sensors params)
   :spotted-grid []
   :uncovered #{}
   :sensors-seen []
   :sensors-unseen []
   :sensor-coverage 0.0 
   :sensor-overlap 0.0})

(def tracking-problem
     (Problem. "Tracking"
               monitor
               {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram}
               generate-truedata
               generate-sensors
               prepared-map
               hypothesize
               get-more-hyps
               commit-decision
               generate-problem-data
               inconsistent
               evaluate
               evaluate-comparative))
