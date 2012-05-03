(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [loom.graph :only [has-edge? weight edges neighbors incoming]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [hyps-equal?]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn generate-kb
  [kb]
  [(new-hyp "KB" :kb :kb false nil 1.0 [] [] "" "" kb)])

(defn get-kb
  [hyps]
  (first (get hyps :kb)))

(defn make-sensor-hyps
  [sensor time-prev time-now hyps]
  (let [kb (get-kb hyps)
        sensor-hyps (map (fn [[sym pos]]
                           (new-hyp "Sens" :sensor :symbol true nil 1.0 [] []
                                    sym (format "Symbol: %c at position %d"
                                                sym pos)
                                    {:sym sym :pos pos}))
                         (sensed-at sensor (inc time-prev)))
        transition-hyps
        (map (fn [[h1 h2]]
               (new-hyp "Trans" :transition :transition true nil 1.0 [h1 h2] []
                        (format "%s/%s" (:sym h1) (:sym h2))
                        (format "Transition: %s/%s, pos-seq: %d-%d"
                                (:sym h1) (:sym h2)
                                (:pos h1) (:pos h2))
                        {:pos-seq [(:pos h1) (:pos h2)]
                         :trans-pos (:pos h1)
                         :sym1 (:sym h1) :sym2 (:sym h2)}))
             (partition 2 1 sensor-hyps))]
    (concat sensor-hyps transition-hyps)))

(defn conflicts
  [hyp1 hyp2]
  (cond
   (or (= :sensor (:type hyp1)) (= :sensor (:type hyp2))) false
   (or (= :transition (:type hyp1)) (= :transition (:type hyp2))) false
   
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [start1 (first (:pos-seq hyp1))
         end1 (last (:pos-seq hyp1))
         start2 (first (:pos-seq hyp2))
         end2 (last (:pos-seq hyp2))]
     (not (or (< end1 start2) (< end2 start1))))
   
   (and (= :word (:type hyp1)) (= :split (:type hyp2)))
   (and (> (:trans-pos hyp2) (first (:pos-seq hyp1)))
        (< (:trans-pos hyp2) (last (:pos-seq hyp1)))
        (not ((set (:explains hyp1)) hyp2)))
   
   (and (= :word (:type hyp2)) (= :split (:type hyp1)))
   (and (> (:trans-pos hyp1) (first (:pos-seq hyp2)))
        (< (:trans-pos hyp1) (last (:pos-seq hyp2)))
        (not ((set (:explains hyp2)) hyp1)))
   
   (or (and (= :split (:type hyp1)) (= :merge (:type hyp2)))
       (and (= :merge (:type hyp1)) (= :split (:type hyp2))))
   (= (:trans-pos hyp1) (:trans-pos hyp2))
   
   :else false))

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

(defn score-split-merge
  [t-hyp kb split?]
  (let [merge-freq (or (weight (:dtg kb) (:sym1 t-hyp) (:sym2 t-hyp)) 0)
        split-freq (get (:wtc kb) [(:sym1 t-hyp) (:sym2 t-hyp)] 0)
        end-prob (/ (double (or (weight (:dtg kb) (:sym1 t-hyp) "end") 0))
                    (double (let [w (reduce + (map #(weight (:dtg kb) (:sym1 t-hyp) %)
                                                   (neighbors (:dtg kb) (:sym1 t-hyp))))]
                              (if (= w 0) 1 w))))
        start-prob (/ (double (or (weight (:dtg kb) "start" (:sym2 t-hyp)) 0))
                      (let [w (reduce + (map #(weight (:dtg kb) % (:sym2 t-hyp))
                                             (incoming (:dtg kb) (:sym2 t-hyp))))]
                        (if (= w 0) 1 w)))
        end-start-prob (if (> (Math/abs (- 0.5 end-prob)) (Math/abs (- 0.5 start-prob)))
                         end-prob start-prob)]
    (if (= 0 (+ split-freq merge-freq))
      (if split? end-start-prob (- 1.0 end-start-prob))
      (if split? (double (/ split-freq (+ split-freq merge-freq)))
          (double (/ merge-freq (+ split-freq merge-freq)))))))

(defn hypothesize
  [forced-hyps accepted hyps]
  (let [kb (get-kb hyps)
        hyp-types (set (str/split (:HypTypes params) #","))
        sensor-hyps (vec (sort-by :pos (filter #(= :sensor (:type %)) forced-hyps)))
        transition-hyps (sort-by :trans-pos (filter #(= :transition (:type %)) forced-hyps))
        split-hyps (map (fn [t-hyp]
                          (new-hyp "Split" :split :split true conflicts
                                   (score-split-merge t-hyp kb true)
                                   [t-hyp] [] (format "%s-%s" (:sym1 t-hyp)
                                                      (:sym2 t-hyp))
                                   (format "Split of %s-%s at %d"
                                           (:sym1 t-hyp)
                                           (:sym2 t-hyp)
                                           (:trans-pos t-hyp))
                                   {:trans-pos (:trans-pos t-hyp)}))
                        transition-hyps)
        merge-hyps (map (fn [t-hyp]
                          (new-hyp "Merge" :merge :merge true conflicts
                                   (score-split-merge t-hyp kb false)
                                   [t-hyp] [] (format "%s+%s" (:sym1 t-hyp) (:sym2 t-hyp))
                                   (format "Merge of %s+%s at %d"
                                           (:sym1 t-hyp)
                                           (:sym2 t-hyp)
                                           (:trans-pos t-hyp))
                                   {:trans-pos (:trans-pos t-hyp)}))
                        transition-hyps)
        sym-string (apply str (map :sym sensor-hyps))
        words (map (fn [[w i]] (subvec sensor-hyps i (+ i (count w))))
                   (find-dict-words sym-string (:dictionary-regex kb)))
        word-hyps
        (map (fn [s-hyps]
               (let [word (apply str (map :sym s-hyps))
                     pos-seq (map :pos s-hyps)]
                 (new-hyp "Word" :word :word false conflicts
                          (/ (double (get (:unigram-model kb) [word]))
                             (double (:word-count kb)))
                          (concat s-hyps
                                  (let [m-hyps (filter #(and (<= (:trans-pos %)
                                                                 (last pos-seq))
                                                             (>= (:trans-pos %)
                                                                 (first pos-seq)))
                                                       (sort-by :trans-pos merge-hyps))]
                                    (if (= (dec (count sym-string))
                                           (last pos-seq))
                                      m-hyps (butlast m-hyps))))
                          [] word
                          (format "Word: %s, pos-seq: %s" word
                                  (str/join ", " (map str pos-seq)))
                          {:pos-seq pos-seq :word word})))
             words)
        bigram-word-hyps
        (map (fn [[wh1 wh2]]
               (let [pos-seq (concat (:pos-seq wh1) (:pos-seq wh2))]
                 (new-hyp "BiWord" :word :biword false conflicts
                          (/ (double (get (:bigram-model kb) [(:word wh1) (:word wh2)]))
                             (double (:word-count kb)))
                          (concat (:explains wh1)
                                  (:explains wh2)
                                  (filter #(= (last (:pos-seq wh1))
                                              (:trans-pos %))
                                          split-hyps))
                          [] (format "%s __ %s" (:word wh1) (:word wh2))
                          (format "Bigram word: %s __ %s, pos-seq: %s"
                                  (:word wh1) (:word wh2)
                                  (str/join ", " (map str pos-seq)))
                          {:pos-seq pos-seq :words [(:word wh1) (:word wh2)]})))
             (filter #(get (:bigram-model kb) [(:word (first %)) (:word (second %))])
                     (partition 2 1 (sort-by (comp first :pos-seq) word-hyps))))]
    (concat merge-hyps split-hyps
            (if (hyp-types "words") word-hyps [])
            (if (hyp-types "biwords") bigram-word-hyps []))))
