(ns retrospect.problems.words.player
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing JSpinner SpinnerNumberModel))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [get-history]])
  (:use [retrospect.state]))

(def fscore-label (label ""))
(def oovrecall-label (label ""))
(def unexp-label (label ""))
(def noexp-label (label ""))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 0 5 0)
          :gridy 0 :gridx 0
          _ (label "FScore:")
          :gridx 1
          _ fscore-label
          :gridy 1 :gridx 0
          _ (label "OOVRecall:")
          :gridx 1
          _ oovrecall-label
          :gridy 2 :gridx 0
          _ (label "Unexplained:")
          :gridx 1
          _ unexp-label
          :gridy 3 :gridx 0
          _ (label "NoExplainers:")
          :gridx 1
          _ noexp-label]))

(defn player-update-stats
  []
  (if (> @time-now 0)
    (let [t (int (/ (dec @time-now) (:StepsBetween params)))
          results (get (:results @or-state) t)]
      (. fscore-label (setText (format "%.2f" (:FScore results))))
      (. oovrecall-label (setText (format "%.2f" (:OOVRecall results))))
      (. unexp-label (setText (format "%.2f" (:UnexplainedPct results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do (. fscore-label (setText "N/A"))
        (. oovrecall-label (setText "N/A"))
        (. unexp-label (setText "N/A"))
        (. noexp-label (setText "N/A")))))

(defn format-truedata-log
  [log sb pre]
  (loop [out (vec (format "(%s) " (apply str pre))) 
         s (seq log)
         ;; (+ 0 ...) to prevent error with boxing & recur
         i (+ 0 (count pre))] 
    (cond (empty? s) (apply str out)
          ;; skip spaces (add to output, but don't "count")
          (= \ (first s)) (recur (conj out \ ) (rest s) i)
          ;; count maxed out, add | symbol, and add letter from log;
          ;; recur with reset counter
          (= i (dec sb)) (recur (conj out (first s) \|) (rest s) 0)
          ;; count still not maxed out, add letter from log, increment count
          :else (recur (conj out (first s)) (rest s) (inc i)))))

(defn player-get-truedata-log
  []
  (format "%s\n\n%s" (get (:test @truedata) @time-now)
          (apply str (interpose " " (nth (:test-sentences @truedata) @time-now)))))

(defn player-get-problem-log
  []
  (let [accepted (:accepted (:workspace (cur-ep (:est @or-state))))]
    (format "History: %s" (apply str (interpose " " (get-history accepted))))))

(defn player-setup-diagram
  [])

(defn player-update-diagram
  [])
