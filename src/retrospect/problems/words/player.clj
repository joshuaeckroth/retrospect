(ns retrospect.problems.words.player
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing JSpinner SpinnerNumberModel))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [get-history find-oov find-new-symbols]])
  (:use [retrospect.state]))

(def fscore-label (label ""))
(def oovrecall-label (label ""))
(def noexp-label (label ""))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          :gridy 0 :gridx 0
          _ (label "FScore:")
          :gridx 1
          _ fscore-label
          :gridy 1 :gridx 0
          _ (label "OOVRecall:")
          :gridx 1
          _ oovrecall-label
          :gridy 2 :gridx 0
          _ (label "NoExplainers:")
          :gridx 1
          _ noexp-label]))

(defn player-update-stats
  []
  (if-let [results (last (:results (cur-ep (:est @or-state))))]
    (do
      (. fscore-label (setText (format "%.2f" (:FScore results))))
      (. oovrecall-label (setText (format "%.2f" (:OOVRecall results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do (. fscore-label (setText "N/A"))
        (. oovrecall-label (setText "N/A"))
        (. noexp-label (setText "N/A")))))

(defn player-get-truedata-log
  []
  (if (= @time-now 0) ""
      (let [sentence (nth (:test-sentences @truedata) (dec @time-now))]
        (format "%s\n\n%s\n\nOOV: %s\nNew symbols: %s"
                (get (:test @truedata) (dec @time-now))
                (str/join " __ " sentence)
                (str/join ", " (map (fn [[w positions]]
                                      (format "%s (%s)" w (str/join ", " (map str positions))))
                                    (seq (find-oov @truedata @time-now))))
                (str/join ", " (map (fn [[sym positions]]
                                      (format "%s (%s)" sym (str/join ", " (map str positions))))
                                    (seq (find-new-symbols @truedata @time-now))))))))

(defn player-get-problem-log
  []
  (let [accepted (:accepted (:workspace (cur-ep (:est @or-state))))]
    (str/join " __ " (get-history accepted))))

(defn player-setup-diagram
  [])

(defn player-update-diagram
  [])
