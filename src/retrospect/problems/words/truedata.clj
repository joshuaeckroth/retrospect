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
            (let [words-grouped
                  (apply concat (for [i (range 1 (inc (:MaxModelGrams params)))]
                                  (partition i (concat (repeat (dec i) "") sentence))))]
              (reduce (fn [ms ws] (let [m (get ms (count ws) {})
                                        prior (get m ws 0)]
                                    (assoc-in ms [(count ws) ws] (inc prior))))
                      models words-grouped)))
          {} training))

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
        [training test] (split-at (int (* 0.9 (count sentences))) (my-shuffle sentences))
        [training-dict test-dict] (map (fn [sents] (set (apply concat sents)))
                                       [training test])
        training-symbols (set (apply concat training-dict))
        ;; TODO: handle noise
        ambiguous (map #(apply str %) test)
        sym-pair-freqs (frequencies (mapcat (fn [sent] (partition 2 1 (apply str sent)))
                                            training))
        dict-no-comps (loop [dict training-dict]
                        (let [composites (filter second (map #(find-inner-words % dict)
                                                             dict))]
                          (if (empty? composites) dict
                              (recur (apply disj dict (map #(apply str (first %))
                                                           composites))))))
        dict-comps (mapcat #(find-inner-words % dict-no-comps)
                           training-dict)
        dtg (reduce (fn [g word]
                      (let [composites (find-inner-words word dict-no-comps)]
                        (reduce (fn [g2 c]
                                  (reduce (fn [g3 e]
                                            (let [w (or (apply weight g3 e) 0)]
                                              (add-edges g3 (conj e (inc w)))))
                                          g2 (conj (map vec (partition 2 1 c))
                                                   ["start" (first c)]
                                                   [(last c) "end"])))
                                g composites)))
                    (weighted-digraph)
                    (apply concat training))
        wtc (frequencies (mapcat
                          (fn [sent]
                            (mapcat
                             (fn [[w1 w2]]
                               (let [c1 (map last (find-inner-words w1 dict-no-comps))
                                     c2 (map first (find-inner-words w2 dict-no-comps))]
                                 (mapcat (fn [sw1]
                                           (map (fn [sw2] [sw1 sw2]) c2))
                                         c1)))
                             (partition 2 1 sent)))
                          training))
        dict-regex (reduce (fn [m w]
                             (assoc m w (re-pattern (format "(%s)" (Pattern/quote w)))))
                           {} training-dict)
        dict-string (str/join " " (concat [" "] dict-no-comps))
        markov-models (build-markov-models training)
        word-freqs (frequencies (apply concat training))
        prefixes (map first (filter second dict-comps))
        suffixes (map last (filter second dict-comps))
        prefix-suffix-freqs (frequencies (apply concat dict-comps))
        prefixes-freq (frequencies prefixes)
        suffixes-freq (frequencies suffixes)
        prefixes-prob (reduce (fn [m w] (assoc m w (/ (double (get prefixes-freq w))
                                                      (double (+ (get prefix-suffix-freqs w)
                                                                 (get word-freqs w 0))))))
                              {} (keys prefixes-freq))
        suffixes-prob (reduce (fn [m w] (assoc m w (/ (double (get suffixes-freq w))
                                                      (double (+ (get prefix-suffix-freqs w)
                                                                 (get word-freqs w 0))))))
                              {} (keys suffixes-freq))]
    {:training {:sentences training :dictionary training-dict :symbols training-symbols
                :word-count (reduce + (map count (apply concat training)))
                :sym-pair-freqs sym-pair-freqs
                :dtg dtg :wtc wtc
                :dictionary-no-composites dict-no-comps
                :dictionary-string dict-string
                :dictionary-regex dict-regex
                :unigram-model (get markov-models 1)
                :bigram-model (get markov-models 2)
                :prefixes-prob prefixes-prob
                :suffixes-prob suffixes-prob}
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test
     :test-dict test-dict}))
