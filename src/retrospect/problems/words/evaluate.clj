(ns retrospect.problems.words.evaluate
  (:import (misc LevenshteinDistance))
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.state]))

(defn calc-ld
 [seq1 seq2]
 (LevenshteinDistance/ld (into-array String seq1) (into-array String seq2)))

(defn get-truewords-starts
  [truedata time]
  (loop [ws []
         ws-count (count (:prefix (meta truedata)))
         td (:words (meta truedata))]
    (let [c (+ ws-count (count (first td)))]
      (if (< time c) ws 
          (recur (conj ws [(first td) ws-count]) c (rest td))))))

(defn evaluate
  [ep-state sensors truedata]
  (let [accepted (:accepted (:problem-data ep-state))
        truewords-starts (get-truewords-starts truedata (:time ep-state))
        truewords (map first truewords-starts)
        correct-pcts
        (for [hyp (sort-by (comp ffirst :pos-seq :data) accepted)]
          (let [words (:words (:data hyp))
                pos-seqs (:pos-seqs (:data hyp))
                correct (count
                         (filter identity
                                 (for [i (range (count words))]
                                   (let [word (nth words i)
                                         word-start (first (nth pos-seqs i))
                                         tw (ffirst (filter #(= word-start (second %)) truewords-starts))]
                                     (= tw word)))))]
            (double (/ correct (count words)))))]
    {:LD (double (/ (calc-ld (:history (:problem-data ep-state)) truewords)
                    (if (empty? truewords) 1 (count truewords))))
     :Correct (* 100.0 (/ (reduce + 0.0 correct-pcts)
                          (if (empty? correct-pcts) 1 (count correct-pcts))))}))

(defn evaluate-comparative
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:LD :Correct])))
