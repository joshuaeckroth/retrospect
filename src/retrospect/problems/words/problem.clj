(ns retrospect.problems.words.problem
  (:require [retrospect.problem])
  (:import [retrospect.problem Problem])
  (:require [clojure.string :as str]))

(defn read-model-csv
  [file]
  (reduce #(assoc %1 (butlast %2) (last %2)) {}
          (map #(str/split % #",") (str/split-lines (slurp file)))))

(defn generate-problem-data
  [sensors datadir params]
  {:dictionary (str/split-lines (slurp (str datadir "/words/dictionary.txt")))
   :model (read-model-csv (str datadir "/words/model.csv"))})

(def headers
  [:PWC])

(def meta-headers
  [:AvgMetaDiffPWC])

(def comparative-headers
  [:MetaPWC :BasePWC :RatioPWC :IncreasePWC])

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
            export-truedata
            prepared-map
            hypothesize
            get-more-hyps
            commit-decision
            generate-problem-data
            inconsistent
            evaluate
            evaluate-meta
            evaluate-comparative))

