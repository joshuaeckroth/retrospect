(ns retrospect.problems.tracking.problem
  (:require [retrospect problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str])
  (:use [retrospect.problems.tracking.evaluate :only
         [evaluate evaluate-comparative true-hyp?]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [generate-sensors]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [hypothesize commit-decision retract no-explainer-hyps]])
  (:use [retrospect.problems.tracking.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
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
  [truedata sensors]
  {:entities (reduce (fn [es e] (assoc es e [(first (get truedata e))]))
                     {} (keys truedata))
   :entity-biases {}
   :accepted #{}
   :unaccepted #{}
   :believed-movements #{}
   :disbelieved-movements #{}
   :walk-dist (read-walk-dist (str @datadir "/tracking/walks-" (:MaxWalk params) ".txt")) 
   :log [] ;; log is reset each time by commit-decision
   :covered-from #{}
   :covered-to #{}
   :uncovered-from #{}
   :uncovered-to #{}})

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
               identity ;; get-more-hyps
               commit-decision
               retract
               generate-problem-data
               (constantly []) ;; inconsistent
               no-explainer-hyps
               evaluate
               evaluate-comparative
               true-hyp?
               {:Steps 25
                :Threshold 20
                :StepsBetween 6
                :SensorNoise 0
                :BeliefNoise 0
                :GridWidth 20
                :GridHeight 20
                :NumberEntities 6
                :MaxWalk 10
                :SensorSeesColor 70
                :SensorCoverage 100
                :ProbNewEntities 0
                :MetaReasoning "NoMetareasoning"
                :TransitiveExplanation false}))
