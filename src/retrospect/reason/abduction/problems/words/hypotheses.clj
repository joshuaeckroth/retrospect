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
               (new-hyp "Trans" :transition :transition true nil 1.0 [] []
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
   
   (or (and (= :split (:type hyp1)) (or (= :merge (:type hyp2))
                                        (= :merge-noexp (:type hyp2))))
       (and (or (= :merge (:type hyp1))
                (= :merge-noexp (:type hyp2))) (= :split (:type hyp2))))
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

(defn score-split
  [t-hyp kb]
  (let [merge-freq (or (weight (:dtg kb) (:sym1 t-hyp) (:sym2 t-hyp)) 0)
        split-freq (get (:wtc kb) [(:sym1 t-hyp) (:sym2 t-hyp)] 0)
        seen-split-prob (if (< (+ split-freq merge-freq) (:MinMergeSplit params)) 0.5
                            (double (/ split-freq (+ split-freq merge-freq))))
        end-prob (let [w (reduce + (map #(weight (:dtg kb) (:sym1 t-hyp) %)
                                        (neighbors (:dtg kb) (:sym1 t-hyp))))]
                   (if (< w (:MinMergeSplit params)) 0.5
                       (/ (double (or (weight (:dtg kb) (:sym1 t-hyp) "end") 0))
                          (double w))))
        start-prob (let [w (reduce + (map #(weight (:dtg kb) % (:sym2 t-hyp))
                                          (incoming (:dtg kb) (:sym2 t-hyp))))]
                     (if (< w (:MinMergeSplit params)) 0.5
                         (/ (double (or (weight (:dtg kb) "start" (:sym2 t-hyp)) 0))
                            (double w))))]
    [seen-split-prob end-prob start-prob]))

(defn hypothesize
  [forced-hyps accepted hyps]
  (let [kb (get-kb hyps)
        hyp-types (set (str/split (:HypTypes params) #","))
        sensor-hyps (vec (sort-by :pos (filter #(= :sensor (:type %)) forced-hyps)))
        transition-hyps (sort-by :trans-pos (filter #(= :transition (:type %)) forced-hyps))
        merge-hyps (map
                    (fn [t-hyp]
                      (let [scores (score-split t-hyp kb)
                            best-split-prob (last (sort-by #(Math/abs (- 0.5 %)) scores))]
                        (new-hyp "Merge" :merge :merge true conflicts
                                 (- 1.0 best-split-prob)
                                 [t-hyp] [] (format "%s+%s" (:sym1 t-hyp) (:sym2 t-hyp))
                                 (format (str "Merge of %s+%s at %d\n"
                                              "seen-split-prob: %.2f\n"
                                              "end-prob: %.2f\n"
                                              "start-prob: %.2f")
                                         (:sym1 t-hyp)
                                         (:sym2 t-hyp)
                                         (:trans-pos t-hyp)
                                         (nth scores 0) (nth scores 1)
                                         (nth scores 2))
                                 {:trans-pos (:trans-pos t-hyp)
                                  :seen-split-prob (nth scores 0)
                                  :end-prob (nth scores 1)
                                  :start-prob (nth scores 2)})))
                    transition-hyps)
        sym-string (apply str (map :sym sensor-hyps))
        words (map (fn [[w i]] (subvec sensor-hyps i (+ i (count w))))
                   (find-dict-words sym-string (:dictionary-regex kb)))
        word-hyps-no-boosting
        (map (fn [s-hyps]
               (let [word (apply str (map :sym s-hyps))
                     pos-seq (map :pos s-hyps)
                     similar-words (map first (find-substrings
                                               word (:dictionary-string kb)))
                     similar-sum (reduce + (map (fn [w] (get (:unigram-model kb) [w]))
                                                similar-words))]
                 (new-hyp "Word" :word :word false conflicts
                          (/ (double (get (:unigram-model kb) [word]))
                             (double similar-sum))
                          (concat s-hyps
                                  (let [m-hyps (filter #(and (<= (:trans-pos %)
                                                                 (last pos-seq))
                                                             (>= (:trans-pos %)
                                                                 (first pos-seq)))
                                                       (sort-by :trans-pos merge-hyps))]
                                    (if (= (dec (count sym-string))
                                           (last pos-seq))
                                      m-hyps (butlast m-hyps))))
                          [] ;; no boosting here
                          word (format "Word: %s, pos-seq: %s" word
                                       (str/join ", " (map str pos-seq)))
                          {:pos-seq pos-seq :word word
                           :similar-words similar-words :similar-sum similar-sum})))
             (sort-by (comp :pos first) words))
        word-exp-merges (set (filter #(= :merge (:type %))
                                     (mapcat :explains word-hyps-no-boosting)))
        merge-noexp-hyps (map (fn [h] (assoc h :type :merge-noexp :subtype :merge-noexp))
                              (filter (fn [h] (not (word-exp-merges h))) merge-hyps))
        merge-noexp-ids (set (map :id merge-noexp-hyps))
        merge-exp-hyps (filter (fn [h] (not (merge-noexp-ids (:id h)))) merge-hyps)
        split-hyps (map
                    (fn [m-hyp]
                      (let [t-hyp (first (:explains m-hyp))]
                        (new-hyp "Split" :split :split true conflicts
                                 (- 1.0 (:apriori m-hyp))
                                 [t-hyp] [] (format "%s-%s" (:sym1 t-hyp)
                                                    (:sym2 t-hyp))
                                 (format (str "Split of %s-%s at %d\n"
                                              "seen-split-prob: %.2f\n"
                                              "end-prob: %.2f\n"
                                              "start-prob: %.2f")
                                         (:sym1 t-hyp)
                                         (:sym2 t-hyp)
                                         (:trans-pos t-hyp)
                                         (:seen-split-prob m-hyp)
                                         (:end-prob m-hyp)
                                         (:start-prob m-hyp))
                                 {:trans-pos (:trans-pos t-hyp)
                                  :seen-split-prob (:seen-split-prob m-hyp)
                                  :end-prob (:end-prob m-hyp)
                                  :start-prob (:start-prob m-hyp)})))
                    (concat merge-exp-hyps merge-noexp-hyps))
        word-hyps (map (fn [hyp] (assoc hyp :boosts
                                        (filter #(or (= (:trans-pos %) (dec (first (:pos-seq hyp))))
                                                     (= (:trans-pos %) (last (:pos-seq hyp))))
                                                split-hyps)))
                       word-hyps-no-boosting)
        bigram-word-hyps
        (when (hyp-types "biwords")
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
                       (partition 2 1 (sort-by (comp first :pos-seq) word-hyps)))))]
    (concat merge-exp-hyps split-hyps
            (if (hyp-types "merge-noexp") merge-noexp-hyps [])
            (if (hyp-types "words") word-hyps [])
            (if (hyp-types "biwords") bigram-word-hyps []))))
