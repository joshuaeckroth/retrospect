(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [loom.graph :only [has-edge? weight edges]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [hyps-equal?]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn make-sensor-hyps
  [sensor [[s1 s2] [pos1 pos2]] time time-prev time-now]
  [(new-hyp "Sens" :sensor :symbol true nil 1.0 [] []
            (str s1 s2) (format "Symbols: '%c%c' at positions %d,%d"
                                 s1 s2 pos1 pos2)
            {:symbol1 s1 :symbol2 s2 :pos1 pos1 :pos2 pos2})])

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
   (and (<= (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
        (>= (last (:pos-seq hyp1)) (first (:pos-seq hyp2))))
   
   (and (= :word-transition (:type hyp1)) (= :word (:type hyp2)))
   (and (<= (first (:pos-seq hyp2)) (first (:pos-seq hyp1)))
        (>= (last (:pos-seq hyp2)) (first (:pos-seq hyp1))))
   
   (and (= :word (:type hyp1)) (= :char-transition (:type hyp2)))
   (or (= (first (:pos-seq hyp2)) (first (:pos-seq hyp1)))
       (= (dec (first (:pos-seq hyp2))) (last (:pos-seq hyp1))))
   
   (and (= :char-transition (:type hyp1)) (= :word (:type hyp2)))
   (or (= (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
       (= (dec (first (:pos-seq hyp1))) (last (:pos-seq hyp2))))

   (or (and (= :char-transition (:type hyp1)) (= :word-transition (:type hyp2)))
       (and (= :word-transition (:type hyp1)) (= :char-transition (:type hyp2))))
   (= (:explains hyp1) (:explains hyp2))

   :else false))

(defn generate-kb
  [kb]
  [(new-hyp "KB" :kb :kb false conflicts 1.0 [] [] "" "" kb)])

(defn get-kb
  [hyps]
  (first (get hyps :kb)))

(defn find-substrings
  "Finds strings that s is a substring of; returns the strings plus
   the offset within each string where we found s."
  [s dict-str]
  (let [m (re-matcher (re-pattern (format " ([^ ]*(%s)[^ ]*)" (Pattern/quote s))) dict-str)]
    (loop [ws []]
      (if (false? (.find m)) ws
          (recur (conj ws [(second (re-groups m)) (- (.start m 2) (.start m 1))]))))))

(defn find-dict-words
  [sym-string dict-regex]
  (reduce (fn [ws w] (let [m (re-matcher (get dict-regex w) sym-string)]
                       (loop [ws2 ws]
                         (if (false? (.find m)) ws2
                             (recur (conj ws2 [w (.start m 1)]))))))
          [] (keys dict-regex)))

(defn hypothesize
  [sensor-hyps hyps]
  (let [kb (get-kb hyps)
        sensor-hyps-sorted (vec (sort-by :pos1 sensor-hyps))
        sym-string (apply str (map :symbol2 (butlast sensor-hyps-sorted)))
        choose-best (fn [hs h] (let [same-explains (filter #(= (:explains h) (:explains %)) hs)
                                     best? (= h (last (sort-by :apriori same-explains)))]
                                 (when best? h)))
        ;; get sequences of sensor hyps that match known words
        words (map (fn [[w i]] (subvec sensor-hyps-sorted i (inc (+ i (count w)))))
                   (find-dict-words sym-string (:dictionary-regex kb)))
        ;; word hyps for known words
        word-hyps
        (map (fn [s-hyps]
               (let [word (apply str (map :symbol2 (butlast s-hyps)))
                     similar-words (map first (find-substrings word (:dictionary-string kb)))
                     similar-sum (reduce + (map (fn [w] (get (:unigram-model kb) [w]))
                                                similar-words))
                     pos-seq (map :pos2 (butlast s-hyps))]
                 (new-hyp "Word" :word :word false conflicts
                          (/ (double (get (:unigram-model kb) [word]))
                             (double similar-sum))
                          s-hyps [] word (format "Word: %s, pos-seq: %s" word
                                                 (str/join ", " (map str pos-seq)))
                          {:pos-seq pos-seq :word word})))
             words)
        in-word-trans-hyps
        (filter identity
                (map (fn [hyp]
                       (when (has-edge? (:dtg kb) (:symbol1 hyp) (:symbol2 hyp))
                         (new-hyp "TransC" :char-transition :char-transition false conflicts
                                  ;; if this symbol pair is also a word transition, figure out how
                                  ;; often it is vs. how often it is an in-word transition;
                                  ;; otherwise, its score is 1.0
                                  (let [w (weight (:dtg kb) (:symbol1 hyp) (:symbol2 hyp))
                                        c (get (:wtc kb) [(:symbol1 hyp) (:symbol2 hyp)] 0)]
                                    (/ (double w) (double (+ w c))))
                                  
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        word-trans-hyps
        (filter identity
                (map (fn [hyp]
                       (when (get (:wtc kb) [(:symbol1 hyp) (:symbol2 hyp)])
                         (new-hyp "TransW" :word-transition :word-transition false conflicts
                                  ;; if this symbol pair is also an in-word transition,
                                  ;; divide by how often that is the case; otherwise,
                                  ;; its score is 1.0
                                  (let [w (get (:wtc kb) [(:symbol1 hyp) (:symbol2 hyp)])
                                        c (or (weight (:dtg kb) (:symbol1 hyp) (:symbol2 hyp)) 0)]
                                    (/ (double w) (double (+ w c))))
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        start-of-word-hyps
        (filter identity
                (map (fn [hyp]
                       (when (has-edge? (:dtg kb) "start" (:symbol2 hyp))
                         ;; score is how often latter symbol is start of a word vs.
                         ;; how often this symbol pair is an in-word transition;
                         ;; if the pair is never anything but a start->symbol2 transition,
                         ;; its score is 1.0
                         (new-hyp "TransWS" :word-transition :word-transition false conflicts
                                  (let [w (weight (:dtg kb) "start" (:symbol2 hyp))
                                        c (or (weight (:dtg kb) (:symbol1 hyp) (:symbol2 hyp)) 0)]
                                    (/ (double w) (double (+ w c))))
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        end-of-word-hyps
        (filter identity
                (map (fn [hyp]
                       (when (has-edge? (:dtg kb) (:symbol1 hyp) "end")
                         ;; score is how often former symbol is end of a word vs.
                         ;; how often this symbol pair is an in-word transition;
                         ;; if the pair is never anything but a symbol1->end transition,
                         ;; its score is 1.0
                         (new-hyp "TransWE" :word-transition :word-transition false conflicts
                                  (let [w (weight (:dtg kb) (:symbol1 hyp) "end")
                                        c (or (weight (:dtg kb) (:symbol1 hyp) (:symbol2 hyp)) 0)]
                                    (/ (double w) (double (+ w c))))
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        all-word-trans-hyps (concat word-trans-hyps start-of-word-hyps end-of-word-hyps)]
    (concat word-hyps in-word-trans-hyps
            (filter identity (map (partial choose-best all-word-trans-hyps) all-word-trans-hyps)))))

