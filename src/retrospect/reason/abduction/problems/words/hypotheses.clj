(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [loom.graph :only [has-edge? weight edges neighbors incoming]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [hyps-equal? get-words]])
  (:use [retrospect.problems.words.truedata :only
         [add-to-in-word-bigram add-to-wtc
          add-to-unigram-model add-to-bigram-model]])
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
  (map (fn [[[sym1 pos1] [sym2 pos2]]]
         (new-hyp "Trans" :transition :transition true nil 1.0 [] []
                  (format "%s/%s" sym1 sym2)
                  (format "Transition: %s/%s, pos-seq: %d-%d"
                          sym1 sym2 pos1 pos2)
                  {:pos-seq [pos1 pos2]
                   :trans-pos pos1
                   :sym1 sym1 :sym2 sym2}))
       (partition 2 1 (sensed-at sensor (inc time-prev)))))

(defn conflicts
  [hyp1 hyp2]
  (cond
   (or (= :transition (:type hyp1)) (= :transition (:type hyp2))) false
   
   (and (= :word (:type hyp1)) (= :word (:type hyp2)))
   (let [start1 (first (:pos-seq hyp1))
         end1 (last (:pos-seq hyp1))
         start2 (first (:pos-seq hyp2))
         end2 (last (:pos-seq hyp2))]
     (not (or (< end1 start2) (< end2 start1))))
   
   (and (= :word (:type hyp1)) (= :split (:type hyp2)))
   (and (>= (:trans-pos hyp2) (first (:pos-seq hyp1)))
        (<= (:trans-pos hyp2) (dec (last (:pos-seq hyp1)))))

   (and (= :word (:type hyp2)) (= :split (:type hyp1)))
   (and (>= (:trans-pos hyp1) (first (:pos-seq hyp2)))
        (<= (:trans-pos hyp1) (dec (last (:pos-seq hyp2)))))
   
   (and (= :word (:type hyp2)) (= :merge (:type hyp1)))
   (or (= (:trans-pos hyp1) (dec (first (:pos-seq hyp2))))
       (= (:trans-pos hyp1) (last (:pos-seq hyp2))))

   (and (= :word (:type hyp1)) (= :merge (:type hyp2)))
   (or (= (:trans-pos hyp2) (dec (first (:pos-seq hyp1))))
       (= (:trans-pos hyp2) (last (:pos-seq hyp1))))

   (or (and (= :split (:type hyp1)) (= :merge (:type hyp2)))
       (and (= :merge (:type hyp1)) (= :split (:type hyp2))))
   (= (:trans-pos hyp1) (:trans-pos hyp2))
   
   :else false))

(defn find-dict-words
  [sym-string dict-regex]
  (prof :find-dict-words
        (reduce (fn [ws w] (let [m (re-matcher (get dict-regex w) sym-string)]
                             (loop [ws2 ws]
                               (if (false? (.find m)) ws2
                                   (recur (conj ws2 [w (.start m 1)]))))))
                [] (keys dict-regex))))

(defn score-split
  [t-hyp kb]
  (prof :score-split
        (let [merge-freq (let [s (get (:in-word-bigrams kb)
                                      [(:sym1 t-hyp) (:sym2 t-hyp)] 0)]
                           (if (:SplitScorePoss params) (inc s) s))
              split-freq (let [s (get (:wtc kb) [(:sym1 t-hyp) (:sym2 t-hyp)] 0)]
                           (if (:SplitScorePoss params) (inc s) s))
              seen-split-prob (double (/ split-freq
                                         (if (= 0 (+ split-freq merge-freq)) 1
                                             (+ split-freq merge-freq))))
              end-prob (/ (double (let [s (get (:in-word-bigrams kb)
                                               [(:sym1 t-hyp) "end"] 0)]
                                    (if (:SplitScorePoss params) (inc s) s)))
                          (double (let [s (get (:in-word-bigrams kb)
                                               [(:sym1 t-hyp) :out] 0)]
                                    (if (:SplitScorePoss params) (+ 2 s)
                                        (if (= 0 s) 1 s)))))
              start-prob (/ (double (let [s (get (:in-word-bigrams kb)
                                                 ["start" (:sym2 t-hyp)] 0)]
                                      (if (:SplitScorePoss params) (inc s) s)))
                            (double (let [s (get (:in-word-bigrams kb)
                                                 [:in (:sym2 t-hyp)] 0)]
                                      (if (:SplitScorePoss params) (+ 2 s)
                                          (if (= 0 s) 1 s)))))]
          [seen-split-prob end-prob start-prob])))

(defn hypothesize
  [forced-hyps accepted hyps]
  (let [kb (get-kb hyps)
        hyp-types (set (str/split (:HypTypes params) #","))
        transition-hyps (vec (sort-by :trans-pos forced-hyps))
        merge-hyps
        (prof :merge-hyps
              (map
               (fn [t-hyp]
                 (let [scores (score-split t-hyp kb)
                       best-split-prob (cond (= "bestAbs" (:SplitScore params))
                                             (last (sort-by #(Math/abs (- 0.5 %)) scores))
                                             (= "max" (:SplitScore params))
                                             (apply max scores)
                                             (= "min" (:SplitScore params))
                                             (apply min scores)
                                             (= "avg" (:SplitScore params))
                                             (/ (reduce + scores) (count scores))
                                             :else
                                             (last (sort-by #(Math/abs (- 0.5 %)) scores)))]
                   (new-hyp "Merge" :merge :merge false conflicts
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
               transition-hyps))
        split-hyps
        (prof :split-hyps
              (map
               (fn [m-hyp]
                 (let [t-hyp (first (:explains m-hyp))]
                   (new-hyp "Split" :split :split false conflicts
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
               merge-hyps))
        sym-string (apply str (map :sym1 transition-hyps))
        words (map (fn [[w i]] (subvec transition-hyps i (+ i (count w))))
                   (find-dict-words sym-string (:dictionary-regex kb)))
        word-hyps
        (prof :word-hyps
              (filter
               #(not-empty (:explains %))
               (map (fn [t-hyps]
                      (let [word (apply str (map :sym1 t-hyps))
                            pos-seq (map :trans-pos t-hyps)
                            similar-words (map #(apply str (map :sym1 %))
                                               (filter #(and (>= (:trans-pos (first t-hyps))
                                                                 (:trans-pos (first %)))
                                                             (<= (:trans-pos (last t-hyps))
                                                                 (:trans-pos (last %))))
                                                       words))
                            similar-sum (reduce + (map (fn [w] (get (:unigram-model kb) w))
                                                       similar-words))]
                        (new-hyp "Word" :word :word false conflicts
                                 (/ (double (get (:unigram-model kb) word))
                                    (double similar-sum))
                                 t-hyps [] ;; no boosting
                                 word (format "Word: %s, pos-seq: %s\nsimilar: %s" word
                                              (str/join ", " (map str pos-seq))
                                              (str/join ", " similar-words))
                                 {:pos-seq pos-seq :word word
                                  :similar-words similar-words
                                  :similar-sum similar-sum})))
                    (sort-by (comp :trans-pos first) words))))
        bigram-word-hyps
        (when (hyp-types "biwords")
          (map (fn [[wh1 wh2]]
                 (let [pos-seq (concat (:pos-seq wh1) (:pos-seq wh2))]
                   (new-hyp "BiWord" :word :biword false conflicts
                            1.0
                            (set (concat (:explains wh1) (:explains wh2)))
                            [] (format "%s __ %s" (:word wh1) (:word wh2))
                            (format "Bigram word: %s __ %s, pos-seq: %s"
                                    (:word wh1) (:word wh2)
                                    (str/join ", " (map str pos-seq)))
                            {:pos-seq pos-seq :words [(:word wh1) (:word wh2)]})))
               (filter #(and (get (:bigram-model kb) [(:word (first %)) (:word (second %))])
                             (= (inc (last (:pos-seq (first %))))
                                (first (:pos-seq (second %)))))
                       (partition 2 1 (sort-by (comp first :pos-seq) word-hyps)))))]
    (comment (filter (fn [h] (not-any? (set (:explains h)) (mapcat :explains word-hyps)))
                     (concat merge-hyps split-hyps)))
    (concat (if (hyp-types "words") []
                (concat merge-hyps split-hyps))
            (if (hyp-types "words") word-hyps [])
            (if (hyp-types "goodwords") (filter #(>= (:apriori %) 0.95) word-hyps) [])
            (if (hyp-types "biwords") bigram-word-hyps []))))

(defn update-kb
  [accepted unexplained hypotheses]
  (let [transition-hyps (sort-by :trans-pos (get accepted :transition))
        sent (get-words (apply str (map :sym1 transition-hyps))
                        accepted unexplained)
        old-kb (get-kb hypotheses)]
    [(-> old-kb
         (update-in [:sentences] conj sent)
         (update-in [:dictionary] set/union (set sent))
         (update-in [:symbols] set/union (set (mapcat seq sent)))
         (update-in [:in-word-bigrams] add-to-in-word-bigram sent)
         (update-in [:wtc] add-to-wtc sent)
         (update-in [:unigram-model] add-to-unigram-model sent)
         (update-in [:bigram-model] add-to-bigram-model sent))]))
