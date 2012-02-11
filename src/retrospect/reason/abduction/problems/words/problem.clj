(ns retrospect.problems.words.problem
  (:require [retrospect.problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [retrospect.problems.words.evaluate :only
         [evaluate evaluate-comparative true-hyp? hyps-equal?]])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors perturb]])
  (:use [retrospect.problems.words.hypotheses :only
         [hypothesize get-more-hyps commit-decision inconsistent retract
          no-explainer-hyps]])
  (:use [retrospect.problems.words.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.words.monitor :only [monitor]])
  (:use [retrospect.problems.words.prepared :only [prepared-map]])
  (:use [retrospect.problems.words.learning :only [update-features calc-centroid]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn read-model-csv
  [file]
  (let [model (reduce #(assoc %1 (vec (butlast %2)) (Integer/parseInt (last %2))) {}
                      (map #(str/split % #",")
                           (str/split-lines (slurp file :encoding (:Encoding params)))))
        sum (reduce + 0 (vals model))]
    (with-meta model {:sum sum})))

(defn generate-problem-data
  [truedata sensors]
  (let [models (zipmap (range 1 (inc (:MaxModelGrams params)))
                       (for [n (range 1 (inc (:MaxModelGrams params)))]
                         (let [csv (format "%s/words/%s/model-%d.csv"
                                           @datadir (:Dataset params) n)]
                           (read-model-csv csv))))
        ;; the agent knows the most common words
        sorted-dict (reverse (sort-by #(get (get models 1) %)
                                      (map first (keys (get models 1)))))
        ;; the agent knows about all words less than :MinLearnLength
        ;; from the true dictionary
        dict (set/union (set (take (int (* (count sorted-dict)
                                           (/ (:Knowledge params) 100)))
                                   sorted-dict))
                        (set (filter #(< (count %) (:MinLearnLength params))
                                     (:dictionary truedata))))
        limited-models (reduce (fn [ms ngram]
                                 (assoc ms ngram
                                        (reduce (fn [m ws]
                                                  (if (every? dict ws) m
                                                      (dissoc m ws)))
                                                (get ms ngram) (keys (get ms ngram)))))
                               models (keys models))
        features (update-features {} dict)]
    {:dictionary dict
     :avg-word-length (if (empty? dict) 0
                          (double (/ (reduce + (map count dict))
                                     (count dict))))
     :models limited-models
     :features features
     :centroid (calc-centroid features (get limited-models 1))
     :left-off -1
     :indexed-letters []
     :accepted #{}
     :history []}))

(def words-problem
     (Problem. "Words"
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
               inconsistent
               no-explainer-hyps
               evaluate
               evaluate-comparative
               true-hyp?
               hyps-equal?
               perturb
               [:word :noise-word :word-seq :learned-word]
               {:Steps [600 [600]]
                :Threshold [20 (range 0 101 20)]
                :StepsBetween [30 (range 10 101 30)]
                :SensorNoise [0 [0 5 10 15 20]]
                :Dataset "cityu"
                :Encoding "big5"
                :MaxModelGrams [3 (range 1 6)]
                :MinWordLength [3 (range 1 5)]
                :MinLearnLength [5 (range 5 10)]
                :MaxLearnLength [8 (range 8 13)]
                :MetaReasoning ["NoMetareasoning" ["NoMetareasoning" "Batch1"
                                                   "Batch2" "BatchBeginning"]]
                :Knowledge [60 (range 0 101 20)]
                :BelievedKnowledge [60 (range 0 101 20)]
                :Learn [true [true false]]
                :LearnFeatureSize [2 (range 1 4)]
                :MaxLearnedWords [10 [10]]
                :MaxNoisyWords [10 [10]]
                :TransitiveExplanation [true [true false]]
                :AnalyzeSensitivity [false [false]]
                :AnalyzeDeps [false [false]]
                :ProbPerturb [25 [25 50 75]]}))

