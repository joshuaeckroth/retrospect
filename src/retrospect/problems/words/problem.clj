(ns retrospect.problems.words.problem
  (:require [retrospect.problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str])
  (:use [retrospect.problems.words.evaluate :only
         [evaluate evaluate-meta evaluate-comparative]])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors]])
  (:use [retrospect.problems.words.hypotheses :only
         [hypothesize get-more-hyps commit-decision inconsistent]])
  (:use [retrospect.problems.words.player :only
         [player-get-params player-set-params player-get-params-panel
          player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.words.monitor :only [monitor]])
  (:use [retrospect.problems.words.prepared :only [prepared-map]]))

(defn read-model-csv
  [file]
  (reduce #(assoc %1 (butlast %2) (Double/parseDouble (last %2))) {}
          (map #(str/split % #",") (str/split-lines (slurp file)))))

(defn generate-problem-data
  [sensors datadir params]
  {:dictionary (str/split-lines (slurp (str datadir "/words/dictionary.txt")))
   :models (zipmap (range 1 (inc (:MaxModelGrams params)))
                   (for [n (range 1 (inc (:MaxModelGrams params)))]
                     (read-model-csv (str datadir (format "/words/model-%d.csv" n))))) 
   :left-off -1
   :accepted []
   :history []})

(def headers
  [:LD :MaxModelGrams])

(def meta-headers
  [:AvgMetaDiffLD])

(def comparative-headers
  [:MetaLD :BaseLD :RatioLD :IncreaseLD :MaxModelGrams])

(def words-problem
  (Problem. "Words"
            headers
            meta-headers
            comparative-headers
            monitor
            {:get-params-fn player-get-params
            :set-params-fn player-set-params
            :get-params-panel-fn player-get-params-panel
            :get-stats-panel-fn player-get-stats-panel
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
            evaluate-meta
            evaluate-comparative))

