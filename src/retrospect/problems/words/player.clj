(ns retrospect.problems.words.player
  (:import (java.awt GridBagLayout Insets))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.state]))

(def ld-label (label ""))

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
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 0 5 0)
          _ (label "Levenshtein distance:")
          :gridx 1
          _ ld-label]))

(defn player-update-stats
  []
  (if (> @time-now 0)
    (let [t (int (/ (dec @time-now) (:StepsBetween @params)))]
      (. ld-label (setText (str (:LD (get (:results @or-state) t))))))
    (do (. ld-label (setText "N/A")))))

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


