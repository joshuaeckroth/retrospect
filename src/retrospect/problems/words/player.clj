(ns retrospect.problems.words.player
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing JSpinner SpinnerNumberModel))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.state]))

(def ld-label (label ""))
(def correct-label (label ""))
(def unexp-label (label ""))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 0 5 0)
          _ (label "Levenshtein dist:")
          :gridx 1
          _ ld-label
          :gridy 1 :gridx 0
          _ (label "Correct:")
          :gridx 1
          _ correct-label
          :gridy 2 :gridx 0
          _ (label "Unexplained:")
          :gridx 1
          _ unexp-label]))

(defn player-update-stats
  []
  (if (> @time-now 0)
    (let [t (int (/ (dec @time-now) (:StepsBetween params)))
          results (get (:results @or-state) t)]
      (. ld-label (setText (format "%.2f" (:LD results))))
      (. correct-label (setText (format "%.2f%%" (:Correct results))))
      (. unexp-label (setText (format "%.2f%%" (:UnexplainedPct results)))))
    (do (. ld-label (setText "N/A"))
        (. correct-label (setText "N/A"))
        (. unexp-label (setText "N/A")))))

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
  (if (= @time-now 0) ""
      (let [sb (:StepsBetween params)
            pre (:prefix (meta @truedata))
            pre-noisy (:prefix-noisy (meta @truedata))
            am-noisy (take @time-now (:ambiguous-noisy (meta @truedata)))
            log-noisy (format-truedata-log (drop (count pre-noisy) am-noisy) sb pre-noisy)]
        (loop [t (count pre)
               words (:words (meta @truedata))
               log ""]
          (let [remaining (- @time-now t)
                next-word (first words)]
            (cond (= remaining 0) (format "%s\n\n%s" log-noisy (format-truedata-log log sb pre))
                  
                  (< remaining (count next-word))
                  (format "%s\n\n%s" log-noisy
                          (format-truedata-log (str log (if (= "" log) "" " ")
                                                    (subs next-word 0 remaining))
                                               sb pre)) 
                  
                  :else
                  (recur (+ t (count next-word)) (rest words)
                         (str log (if (= "" log) "" " ") next-word))))))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))
        {:keys [predicted active-word history]} pdata]
    (format "History: %s" (apply str (interpose " " history)))))

(defn player-setup-diagram
  [])

(defn player-update-diagram
  [])


