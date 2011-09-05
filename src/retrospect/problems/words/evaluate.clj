(ns retrospect.problems.words.evaluate
  (:import (misc LevenshteinDistance))
  (:use [retrospect.evaluate :only [calc-ratio-increase]]))

(defn calc-ld
 [seq1 seq2]
 (LevenshteinDistance/ld (into-array String seq1) (into-array String seq2)))

(defn get-truewords
  [truedata time]
  (loop [ws []
         ws-count (count (:prefix (meta truedata)))
         td (:words (meta truedata))]
    (let [c (+ ws-count (count (first td)))]
      (if (< time c) ws 
        (recur (conj ws (first td)) c (rest td))))))

(defn evaluate
  [ep-state results sensors truedata params]
  (let [time (:time ep-state)
        truewords (get-truewords truedata (:time ep-state))]
    {:LD (double (/ (calc-ld (:history (:problem-data ep-state)) truewords)
                    (if (empty? truewords) 1 (count truewords)))) }))

(defn evaluate-comparative
  [control-results comparison-results params]
  (calc-ratio-increase control-results comparison-results :LD))
