(ns retrospect.problems.words.player
  (:import (java.awt GridBagLayout Insets))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.problems.words.evaluate :only
         [get-words find-oov]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp get-unexplained]])
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
      (let [sentence (nth (:test-sentences-nonoise @truedata) (dec @time-now))]
        (format "%s\n\n%s\n\nOOV: %s"
           (get (:test-nonoise @truedata) (dec @time-now))
           (str/join " __ " sentence)
           (str/join ", " (map (fn [[w positions]]
                               (format "%s (%s)" w (str/join ", " (map str positions))))
                             (seq (find-oov @truedata @time-now))))))))

(defn player-get-problem-log
  []
  (let [ws (:workspace (cur-ep (:est @or-state)))
        accepted (:accepted ws)
        unexplained (get-unexplained ws)]
    (format "%s\n\n%s"
       (get (:test @truedata) (dec @time-now))
       (str/join " __ " (get-words (partial lookup-hyp ws)
                                   (get (:test @truedata) (dec @time-now))
                                   accepted unexplained)))))

(defn player-setup-diagram
  [])

(defn player-update-diagram
  [])
