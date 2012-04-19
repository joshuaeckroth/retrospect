(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [loom.graph :only [has-edge?]])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [hyps-equal?]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn make-sensor-hyps
  [sensor [symbol pos] time time-prev time-now]
  [(new-hyp "Sens" :sensor :symbol true nil 1.0 [] []
            (str symbol) (format "Symbol: '%c' at position %d" symbol pos)
            {:pos pos :symbol symbol})])

(defn conflicts
  [hyp1 hyp2]
  (cond
   (or (= :sensor (:type hyp1)) (= :sensor (:type hyp2))) false
   (and (= :word-seq (:type hyp1)) (= :word-seq (:type hyp2)))
   (some (fn [n] (or (= (take n (:pos-seqs hyp1))
                        (:pos-seqs hyp2))
                     (= (take-last n (:pos-seqs hyp1))
                        (:pos-seqs hyp2))
                     (= (take n (:pos-seqs hyp2))
                        (:pos-seqs hyp1))
                     (= (take-last n (:pos-seqs hyp2))
                        (:pos-seqs hyp1))))
         (range 1 (inc (min (count (:pos-seqs hyp1))
                            (count (:pos-seqs hyp2))))))
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [start1 (first (:pos-seq hyp1))
         end1 (last (:pos-seq hyp1))
         start2 (first (:pos-seq hyp2))
         end2 (last (:pos-seq hyp2))]
     (not (or (< end1 start2) (< end2 start1))))
   
   (and (= :word (:type hyp1)) (= :word-transition (:type hyp2)))
   (and (< (first (:pos-seq hyp1)) (second (:pos-seq hyp2)))
        (> (last (:pos-seq hyp1)) (first (:pos-seq hyp2))))
   
   (and (= :word-transition (:type hyp1)) (= :word (:type hyp2)))
   (and (< (first (:pos-seq hyp2)) (second (:pos-seq hyp1)))
        (> (last (:pos-seq hyp2)) (first (:pos-seq hyp1))))
   
   (and (= :word (:type hyp1)) (= :char-transition (:type hyp2)))
   (or (= (second (:pos-seq hyp2)) (first (:pos-seq hyp1)))
       (= (first (:pos-seq hyp2)) (last (:pos-seq hyp1))))
   
   (and (= :char-transition (:type hyp1)) (= :word (:type hyp2)))
   (or (= (second (:pos-seq hyp1)) (first (:pos-seq hyp2)))
       (= (first (:pos-seq hyp1)) (last (:pos-seq hyp2))))

   :else false))

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

(comment
  (let [kb (try
             (with-open [r (java.io.PushbackReader.
                            (clojure.java.io/reader
                             (format "%s/words/kb-%s-%d.clj" @datadir
                                     (:Dataset params) (:MaxModelGrams params))))]
               (read r))
             (catch Exception _
               (let [dtc 

                     models (build-markov-models training)
                     model-sums (reduce (fn [m-s n]
                                          (assoc m-s n (double (reduce + (vals (get models n))))))
                                        {} (keys models))
                     substrings (reduce (fn [m w] (assoc m w (filter #(substring? w %) dictionary)))
                                        {} dictionary)
                     symbol-words (reduce (fn [m w] (reduce (fn [m2 sym]
                                                              (if (nil? (get m2 sym))
                                                                (assoc m2 sym #{w})
                                                                (update-in m2 [sym] conj w)))
                                                            m (seq w)))
                                          {} dictionary)
                     dict-counts (map count dictionary)
                     kb {:dtc 

                         :avg-word-length (if (empty? dictionary) 0
                                              (double (/ (reduce + dict-counts)
                                                         (count dictionary))))
                         :max-word-length (apply max dict-counts)
                         :dictionary dictionary
                         :models models
                         :model-sums model-sums
                         :symbol-words symbol-words
                         :substrings substrings}]
                 (spit (format "%s/words/kb-%s-%d.clj" @datadir
                               (:Dataset params) (:MaxModelGrams params))
                       (pr-str kb))
                 kb)))]))

(defn generate-kb
  [kb]
  [(new-hyp "KB" :kb :kb false conflicts 1.0 [] [] "" "" kb)])

(defn get-kb
  [hyps]
  (first (get hyps :kb)))

(defn hypothesize
  [sensor-hyps hyps]
  (let [kb (get-kb hyps)
        sensor-hyps-sorted (vec (sort-by :pos sensor-hyps))
        sym-string (apply str (map :symbol sensor-hyps-sorted))
        pairs (concat [["start" (first sensor-hyps-sorted)]]
                      (partition 2 1 sensor-hyps-sorted)
                      [[(last sensor-hyps-sorted) "end"]])
        words (filter identity
                      (mapcat
                       (fn [w] (let [m (re-matcher (re-pattern (format "(%s)" (Pattern/quote w)))
                                                   sym-string)]
                                 (when (re-find m)
                                   (map (fn [g] (subvec sensor-hyps-sorted (.start m g)
                                                        (+ (.start m g) (count w))))
                                        (range 1 (inc (.groupCount m)))))))
                       (:dictionary kb)))]
    (concat
     ;; word hyps
     (map (fn [s-hyps]
            (let [word (apply str (map :symbol s-hyps))]
              (new-hyp "Word" :word :word false conflicts
                       0.50 s-hyps [] word ""
                       {:pos-seq (map :pos s-hyps)
                        :word word})))
          words)
     ;; word-transition pair hyps
     (filter identity
             (map (fn [[sh1 sh2]]
                    (when ((:wtc kb) [(:symbol sh1) (:symbol sh2)])
                      (new-hyp "TransW" :word-transition :word-transition false conflicts
                               0.50 [sh1 sh2] [] "" ""
                               {:pos-seq [(:pos sh1) (:pos sh2)]})))
                  (butlast (rest pairs))))
     ;; in-word-transition pair hyps
     (filter identity
             (map (fn [[sh1 sh2]]
                    (when (has-edge? (:dtg kb) (:symbol sh1) (:symbol sh2))
                      (new-hyp "TransC" :char-transition :char-transition false conflicts
                               0.50 [sh1 sh2] [] "" ""
                               {:pos-seq [(:pos sh1) (:pos sh2)]})))
                  (butlast (rest pairs)))))))

