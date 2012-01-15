(ns retrospect.problems.tracking.problem
  (:require [retrospect problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str])
  (:use [retrospect.problems.tracking.evaluate :only
         [evaluate evaluate-comparative true-hyp? hyps-equal?]])
  (:use [retrospect.problems.tracking.truedata :only
         [generate-truedata]])
  (:use [retrospect.problems.tracking.sensors :only
         [generate-sensors perturb]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [hypothesize get-more-hyps commit-decision retract no-explainer-hyps]])
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
   :entity-biases (if-not (:KnowBiases params) {}
                          (reduce (fn [m e]
                                    (assoc m e {:bias (:bias (first (get truedata e)))}))
                                  {} (keys truedata)))
   :accepted #{}
   :unaccepted #{}
   :believed-movements #{}
   :disbelieved-movements #{}
   :walk-dist (read-walk-dist (format "%s/tracking/walks-%d.txt" @datadir (:MaxWalk params)))
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
               get-more-hyps
               commit-decision
               retract
               generate-problem-data
               (constantly []) ;; inconsistent
               no-explainer-hyps
               evaluate
               evaluate-comparative
               true-hyp?
               hyps-equal?
               perturb
               [:movement :path :location :bias]
               {:Steps [30 [30]]
                :Threshold [20 (range 0 101 20)]
                :StepsBetween [6 (range 1 11)]
                :SensorNoise [0 [0]]
                :BeliefNoise [0 [0]]
                :GridWidth [20 [20]]
                :GridHeight [20 [20]]
                :NumberEntities [6 (range 1 11)]
                :PathBranches [6 (range 1 9 2)]
                :KnowBiases [true [true false]]
                :MaxWalk [10 (range 1 11)]
                :WindowSize [10 (range 5 31 5)]
                :SensorSeesColor [70 (range 0 101 20)]
                :SensorCoverage [100 [100]]
                :ProbNewEntities [0 [0]]
                :MetaReasoning ["NoMetareasoning" ["NoMetareasoning" "Batch1"
                                                   "Batch2" "Batch3" "BatchBeginning"]]
                :TransitiveExplanation [false [true false]]
                :AnalyzeSensitivity [false [true false]]
                :ProbPerturb [25 [25 50 75]]}))
