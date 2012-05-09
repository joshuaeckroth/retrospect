(ns retrospect.problems.words.truedata
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [loom.graph :only [weighted-digraph weight add-edges edges]])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn build-markov-models
  "Build a Markov n-gram model of word transitions."
  [training]
  (prof :build-markov-models
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
                  {} training))))

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
  (profile
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
         [in-word-bigrams wtc unigram-model]
         (prof :iwb-wtc-ugm
               (reduce (fn [[iwb wtc ugm] sent]
                         (let [iwb2
                               (reduce
                                (fn [m w]
                                  (reduce (fn [m2 [c1 c2]]
                                            (let [p (get m2 [c1 c2] 0)
                                                  p-in (get m2 [:in c2] 0)
                                                  p-out (get m2 [c1 :out] 0)]
                                              (-> m2
                                                  (assoc [c1 c2] (inc p))
                                                  (assoc [:in c2] (inc p-in))
                                                  (assoc [c1 :out] (inc p-out)))))
                                          m (conj (partition 2 1 (seq w))
                                                  ["start" (first w)]
                                                  [(last w) "end"])))
                                iwb sent)
                               wtc2
                               (reduce (fn [m [w1 w2]]
                                         (let [p (get m [(last w1) (first w2)] 0)]
                                           (assoc m [(last w1) (first w2)] (inc p))))
                                       wtc (partition 2 1 sent))
                               ugm2
                               (reduce (fn [m w]
                                         (let [p (get m w 0)]
                                           (assoc m w (inc p))))
                                       ugm sent)]
                           [iwb2 wtc2 ugm2]))
                       [{} {} {}] training))
         dict-regex (reduce (fn [m w]
                              (assoc m w (re-pattern (format "(%s)" (Pattern/quote w)))))
                            {} training-dict)]
     {:training {:sentences training :dictionary training-dict :symbols training-symbols
                 :in-word-bigrams in-word-bigrams :wtc wtc
                 :dictionary-regex dict-regex
                 :unigram-model unigram-model}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-dict test-dict})))
