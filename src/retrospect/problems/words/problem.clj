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
  [file dict]
  (let [model
        (reduce #(assoc %1 (vec (butlast %2)) (Integer/parseInt (last %2))) {}
                (map #(str/split % #",") (str/split-lines (slurp file))))
        ;; select only those n-grams that have all their words from dict
        reduced-model (select-keys model (filter #(every? dict %) (keys model)))
        sum (reduce + 0 (vals reduced-model))]
    (with-meta reduced-model {:sum sum})))

(defn generate-problem-data
  [truedata sensors]
  (let [models (zipmap (range 1 (inc (:MaxModelGrams params)))
                       (for [n (range 1 (inc (:MaxModelGrams params)))]
                         (let [csv (str @datadir (format "/words/model-%d.csv" n))]
                           (read-model-csv csv (:dictionary (meta truedata))))))
        sorted-dict (sort-by #(get (get models 1) %) (:dictionary (meta truedata)))
        ;; the agent always "knows" about all words less than :MinLearnLength
        dict (set/union (set (take (int (* (count sorted-dict)
                                           (/ (:Knowledge params) 100)))
                                   sorted-dict))
                        (set (filter #(< (count %) (:MinLearnLength params))
                                     sorted-dict)))
        limited-models (reduce (fn [ms ngram]
                                 (assoc ms ngram
                                        (reduce (fn [m ws] (if (every? dict ws) m
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
               [:word :word-seq :learned-word]
               {:Steps 600
                :Threshold 20
                :StepsBetween 30
                :SensorNoise 0
                :BeliefNoise 0
                :MaxModelGrams 3
                :MinWordLength 3
                :MinLearnLength 5
                :MaxLearnLength 8
                :MetaReasoning "NoMetaReasoning"
                :Knowledge 60
                :BelievedKnowledge 60
                :Learn true
                :LearnFeatureSize 2
                :TransitiveExplanation true
                :AnalyzeSensitivity false}))

