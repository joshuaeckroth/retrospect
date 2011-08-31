(ns retrospect.problems.words.player
  (:import (java.awt GridBagLayout Insets))
  (:import (javax.swing JSpinner SpinnerNumberModel))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:require [clojure.string :as str])
  (:use [retrospect.state]))

(def ld-label (label ""))

(def param-spinners
  {:MaxModelGrams (JSpinner. (SpinnerNumberModel. 1 1 10 1))})

(defn player-get-params
  []
  (for [k (keys param-spinners)]
    [k (->> (k param-spinners) .getModel .getNumber .intValue)]))

(defn player-set-params
  []
  (doseq [k (keys param-spinners)]
    (. (k param-spinners) setValue (k @params))))

(defn player-get-params-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "MaxModelGrams:")
          :gridx 1
          _ (:MaxModelGrams param-spinners)
          :gridy 1 :weighty 1.0
          _ (panel)]))

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

(defn format-truedata-log
  [noisy log sb prefix]
  (let [trueends (map (fn [chunk] (second (last chunk)))
                      (partition sb (take @time-now noisy)))]
    (loop [out (vec (format "%s\n\n(%s) "
                            (apply str (map first (take @time-now noisy)))
                            (apply str prefix)))
           s (seq log)
           ends trueends
           lastend (dec (count prefix))]
      (cond (empty? ends) (apply str out)
            (= lastend (first ends)) (recur (conj out \|) s (rest ends) lastend)
            (= \ (first s)) (recur (conj out \ ) (rest s) ends lastend)
            :else (recur (conj out (first s)) (rest s) ends (inc lastend))))))

(defn player-get-truedata-log
  []
  (if (= @time-now 0) ""
      (let [noisy (:noisy (meta @truedata))
            ;; amount of truedata letters presented
            truetime (second (nth noisy @time-now))
            prefix (:prefix (meta @truedata))]
        (loop [t (count prefix)
               words (:words (meta @truedata))
               log ""]
          (let [remaining (- truetime t)
                next-word (first words)]
            (cond (= remaining 0)
                  (format-truedata-log noisy log (:StepsBetween @params) prefix) 
                  
                  (< remaining (count next-word))
                  (format-truedata-log noisy (str log (if (= "" log) "" " ")
                                                  (subs next-word 0 remaining))
                                       (:StepsBetween @params) prefix) 
                  
                  :else
                  (recur (+ t (count next-word)) (rest words)
                         (str log (if (= "" log) "" " ") next-word))))))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))
        {:keys [predicted active-word history]} pdata]
    (format "History: %s" (apply str (interpose " " history)))))

(defn player-setup-diagram
  [p])

(defn player-update-diagram
  [])


