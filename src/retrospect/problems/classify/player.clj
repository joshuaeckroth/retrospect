(ns retrospect.problems.classify.player
  (:require [clojure.string :as str])
  (:import (java.awt GridBagLayout Insets))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.problems.classify.evaluate :only [chosen-cats]])
  (:use [retrospect.state]))

(def fscore-label (label ""))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          :gridy 0 :gridx 0
          _ (label "FScore:")
          :gridx 1
          _ fscore-label]))

(defn player-update-stats
  []
  (if-let [results (last (:results (cur-ep (:est @or-state))))]
    (do
      (. fscore-label (setText (format "%.2f" (:FScore results)))))
    (do (. fscore-label (setText "N/A")))))

(defn player-get-truedata-log
  []
  (if (= @time-now 0) ""
      (let [[docid _] (get (:test @truedata) (dec @time-now))]
        (format "%s true categories: %s" docid
           (str/join ", " (sort (get (:cats @truedata) docid)))))))

(defn player-get-problem-log
  []
  (let [ws (:workspace (cur-ep (:est @or-state)))
        doccats (chosen-cats ws)]
    (str/join "\n" (map (fn [[docid cats]]
                        (format "%s chosen categories: %s" docid
                           (str/join ", " (sort cats))))
                      (sort-by first (seq doccats))))))

(defn player-setup-diagram
  [])

(defn player-update-diagram
  [])


