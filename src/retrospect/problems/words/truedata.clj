(ns retrospect.problems.words.truedata
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [loom.graph :only [weighted-digraph weight add-edges edges]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn build-markov-models
  "Build a Markov n-gram model of word transitions."
  [training]
  (let [hyp-types (set (str/split (:HypTypes params) #","))]
    (reduce (fn [models sentence]
              (let [words-grouped
                    (apply concat (for [i (if (hyp-types "biwords")
                                            [1 2] [1])]
                                    (partition i (concat (repeat (dec i) "") sentence))))]
                (reduce (fn [ms ws] (let [m (get ms (count ws) {})
                                          prior (get m ws 0)]
                                      (assoc-in ms [(count ws) ws] (inc prior))))
                        models words-grouped)))
            {} training)))

(defn find-inner-words
  [word dictionary]
  (if (empty? word) []
      (let [ws (filter dictionary (map (fn [i] (apply str (take i word)))
                                       (range 1 (inc (count word)))))]
        (concat
         (if (dictionary word) [[word]] [])
         (mapcat (fn [w] (let [rest-word (apply str (drop (count w) word))
                               rest-inner-words (find-inner-words rest-word dictionary)]
                           (map #(concat [w] %) rest-inner-words)))
                 ws)))))

(defn generate-truedata
  []
  ;; attached a space at the front and end of each sentence to
  ;; facilitate sensor hyps that have pairs of symbols
  (let [sentences (map (fn [sent] (filter not-empty (str/split sent #"[\s　]+")))
                       (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                       @datadir (:Dataset params))
                                               :encoding "utf-8")))
        [training test2] (split-at (int (* 0.9 (count sentences))) (my-shuffle sentences))
        test (if (:ShortFirst params) (sort-by count test2) test2)
        [training-dict test-dict] (map (fn [sents] (set (apply concat sents)))
                                       [training test])
        training-symbols (set (apply concat training-dict))
        ;; TODO: handle noise
        ambiguous (map #(apply str %) test)
        dtg (reduce (fn [g word]
                      (reduce (fn [g2 pair]
                                (let [w (or (apply weight g2 pair) 0)]
                                  (add-edges g2 (conj pair (inc w)))))
                              g (conj (map vec (partition 2 1 (seq word)))
                                      ["start" (first word)]
                                      [(last word) "end"])))
                    (weighted-digraph)
                    (apply concat training))
        wtc (frequencies (mapcat
                          (fn [sent]
                            (mapcat (fn [[w1 w2]] [(last w1) (first w2)])
                                    (partition 2 1 sent)))
                          training))
        dict-regex (reduce (fn [m w]
                             (assoc m w (re-pattern (format "(%s)" (Pattern/quote w)))))
                           {} training-dict)
        dict-string (str/join " " (concat [" "] training-dict))
        markov-models (build-markov-models training)]
    {:training {:sentences training :dictionary training-dict :symbols training-symbols
                :word-count (reduce + (map count (apply concat training)))
                :dtg dtg :wtc wtc
                :dictionary-string dict-string
                :dictionary-regex dict-regex
                :unigram-model (get markov-models 1)
                :bigram-model (get markov-models 2)}
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test
     :test-dict test-dict}))
