(ns retrospect.problems.words.problem
  (:require [retrospect.problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str])
  (:use [retrospect.problems.words.evaluate :only
         [evaluate evaluate-comparative]])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors]])
  (:use [retrospect.problems.words.hypotheses :only
         [hypothesize get-more-hyps commit-decision inconsistent]])
  (:use [retrospect.problems.words.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.words.monitor :only [monitor]])
  (:use [retrospect.problems.words.prepared :only [prepared-map]])
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
  (let [full-dict (my-shuffle (str/split-lines (slurp (str @datadir "/words/dictionary.txt"))))
        dict (set (take (int (* (count full-dict) (/ (:Knowledge params) 100))) full-dict))]
    {:dictionary dict
     :models (zipmap (range 1 (inc (:MaxModelGrams params)))
                     (for [n (range 1 (inc (:MaxModelGrams params)))]
                       (let [csv (str @datadir (format "/words/model-%d.csv" n))]
                         (read-model-csv csv dict))))
     :left-off -1
     :indexed-letters []
     :accepted []
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
               generate-problem-data
               inconsistent
               evaluate
               evaluate-comparative
               {:Steps 120
                :Threshold 20
                :StepsBetween 30
                :SensorNoise 0
                :BeliefNoise 0
                :MaxModelGrams 3
                :MetaReasoning "NoMetaReasoning"
                :Knowledge 100
                :BelievedKnowledge 100
                :Learn false
                :TransitiveExplanation true}))

