(ns retrospect.problems.words.player
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.state]))

(defn player-get-params
  []
  [])

(defn player-set-params
  [])

(defn player-get-params-panel
  []
  (panel))

(defn player-get-stats-panel
  []
  (panel))

(defn player-update-stats
  [])

(defn player-get-truedata-log
  []
  (if (= @time-now 0) ""
    (loop [t 0
           words (:words (meta @truedata))
           log ""]
      (let [remaining (- @time-now t)
            next-word (first words)]
        (cond (= remaining 0) log

              (< remaining (count next-word))
              (str log " " (subs next-word 0 remaining)) 

              :else
              (recur (+ t (count next-word)) (rest words)
                     (str log " " next-word)))))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))
        {:keys [predicted active-word history]} pdata]
    (format "History: %s\n\nActive word: %s\nPredicted: %s"
            (apply str (interpose " " history))
            active-word (str/join predicted))))

(defn player-setup-diagram
  [p])

(defn player-update-diagram
  [])


