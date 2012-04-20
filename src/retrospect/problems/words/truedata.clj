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
  (reduce (fn [models sentence]
            (let [words-grouped (apply concat (for [i (range 1 (inc (:MaxModelGrams params)))]
                                                (partition i (concat (repeat (dec i) "") sentence))))]
              (reduce (fn [ms ws] (let [m (get ms (count ws) {})
                                        prior (get m ws 0)]
                                    (assoc-in ms [(count ws) ws] (inc prior))))
                      models words-grouped)))
          {} training))

(defn generate-truedata
  []
  ;; attached a space at the front and end of each sentence to
  ;; facilitate sensor hyps that have pairs of symbols
  (let [sentences (map (fn [sent] (concat [" "] (filter not-empty (str/split sent #"\s+")) [" "]))
                       (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                       @datadir (:Dataset params))
                                               :encoding "utf-8")))
        [training test] (split-at (int (* 0.9 (count sentences))) (my-shuffle sentences))
        [training-dict test-dict] (map (fn [sents] (set (apply concat sents)))
                                       [training test])
        training-symbols (set (apply concat training-dict))
        ;; TODO: handle noise
        ambiguous (map #(apply str %) test)
        dtg (reduce (fn [g word] (reduce (fn [g2 e]
                                           (let [w (or (apply weight g e) 0)]
                                             (add-edges g2 (conj e (inc w)))))
                                         g (conj (map vec (partition 2 1 word))
                                                 ["start" (first word)]
                                                 [(last word) "end"])))
                    (weighted-digraph)
                    training-dict)
        wtc (frequencies (mapcat (fn [sent] (map (fn [[w1 w2]] [(last w1) (first w2)])
                                                 (partition 2 1 sent)))
                                 training))]
    {:training {:sentences training :dictionary training-dict :symbols training-symbols
                :dtg dtg :wtc wtc
                :dictionary-string (str/join " " (concat [" "] training-dict))
                :dictionary-regex (reduce (fn [m w] (assoc m w (re-pattern (format "(%s)" (Pattern/quote w)))))
                                          {} training-dict)
                :unigram-model (get (build-markov-models training) 1)}
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test
     :test-dict test-dict}))
