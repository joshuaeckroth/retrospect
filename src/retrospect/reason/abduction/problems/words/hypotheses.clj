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
                           (new-hyp "Sens" :sensor :symbol false nil 1.0 [] []
                                    sym (format "Symbol: %c at position %d"
                                                sym pos)
                                    {:sym sym :pos pos}))
                         (sensed-at sensor (inc time-prev)))
        ;; get sequences of sensor hyps that match known subwords;
        ;; this algorithm is simple (greedy left-to-right) because we
        ;; don't have to worry about overlap
        subwords (loop [s-hyps sensor-hyps
                        subwords []]
                   (if (empty? s-hyps) subwords
                       (let [i (first (filter #((:dictionary-no-composites kb)
                                                (apply str (map :sym (take % s-hyps))))
                                              (range 1 (inc (count s-hyps)))))]
                         (if i (recur (drop i s-hyps) (conj subwords (take i s-hyps)))
                             ;; must be a new symbol
                             (recur (rest s-hyps) (conj subwords (take 1 s-hyps)))))))
        subword-hyps
        (map (fn [s-hyps]
               (let [subword (apply str (map :sym s-hyps))
                     pos-seq (map :pos s-hyps)]
                 (new-hyp "Subword" :subword :subword true nil
                          1.0 s-hyps [] subword
                          (format "Subword: %s, pos-seq: %s" subword
                                  (str/join ", " (map str pos-seq)))
                          {:pos-seq pos-seq :subword subword})))
             subwords)
        transition-hyps
        (map (fn [[h1 h2]]
               (new-hyp "Trans" :transition :transition true nil
                        1.0 (concat (:explains h1) (:explains h2)) []
                        (format "%s/%s" (:subword h1) (:subword h2))
                        (format "Transition: %s/%s, pos-seq: %d-%d"
                                (:subword h1) (:subword h2)
                                (first (:pos-seq h1)) (last (:pos-seq h2)))
                        {:pos-seq (concat (:pos-seq h1) (:pos-seq h2))
                         :trans-pos (last (:pos-seq h1))
                         :subword1 (:subword h1) :subword2 (:subword h2)}))
             (partition 2 1 subword-hyps))]
    (concat sensor-hyps subword-hyps transition-hyps)))

(defn conflicts
  [hyp1 hyp2]
  (cond
   (or (= :sensor (:type hyp1)) (= :sensor (:type hyp2))) false
   (or (= :subword (:type hyp1)) (= :subword (:type hyp2))) false
   
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
   
   (and (= :word (:type hyp1)) (= :split (:type hyp2)))
   (and (> (:trans-pos hyp2) (first (:pos-seq hyp1)))
        (< (:trans-pos hyp2) (last (:pos-seq hyp1))))
   
   (and (= :word (:type hyp2)) (= :split (:type hyp1)))
   (and (> (:trans-pos hyp1) (first (:pos-seq hyp2)))
        (< (:trans-pos hyp1) (last (:pos-seq hyp2))))
   
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

(comment
  (or (weight (:dtg kb) "start" (:subword2 t-hyp)) 0)
  (or (weight (:dtg kb) (:subword1 t-hyp) "end") 0))

(defn score-split-merge
  [t-hyp kb split?]
  (let [merge-freq (or (weight (:dtg kb) (:subword1 t-hyp)
                               (:subword2 t-hyp)) 0)
        split-freq (get (:wtc kb) [(:subword1 t-hyp)
                                   (:subword2 t-hyp)] 0)]
    (if (= 0 (+ split-freq merge-freq)) 0.0
        (if split? (double (/ split-freq (+ split-freq merge-freq)))
            (double (/ merge-freq (+ split-freq merge-freq)))))))

(defn hypothesize
  [forced-hyps accepted hyps]
  (let [kb (get-kb hyps)
        sensor-hyps (vec (sort-by :pos (filter #(= :sensor (:type %)) forced-hyps)))
        transition-hyps (sort-by :trans-pos (filter #(= :transition (:type %)) forced-hyps))
        split-hyps (map (fn [t-hyp]
                          (new-hyp "Split" :split :split false conflicts
                                   (score-split-merge t-hyp kb true)
                                   [t-hyp] [] (format "%s-%s" (:subword1 t-hyp)
                                                      (:subword2 t-hyp))
                                   (format "Split of %s-%s at %d"
                                           (:subword1 t-hyp)
                                           (:subword2 t-hyp)
                                           (:trans-pos t-hyp))
                                   {:trans-pos (:trans-pos t-hyp)}))
                        transition-hyps)
        merge-hyps (map (fn [t-hyp]
                          (new-hyp "Merge" :merge :merge true conflicts
                                   (score-split-merge t-hyp kb false)
                                   [t-hyp] [] (format "%s+%s" (:subword1 t-hyp)
                                                      (:subword2 t-hyp))
                                   (format "Merge of %s+%s at %d"
                                           (:subword1 t-hyp)
                                           (:subword2 t-hyp)
                                           (:trans-pos t-hyp))
                                   {:trans-pos (:trans-pos t-hyp)}))
                        transition-hyps)
        sym-string (apply str (map :sym sensor-hyps))
        words (map (fn [[w i]]
                     (let [s-hyps (set (subvec sensor-hyps i (+ i (count w))))]
                       (sort-by (comp first :pos-seq)
                                (filter (fn [sw-hyp] (some s-hyps (:explains sw-hyp)))
                                        (get hyps :subword)))))
                   (find-dict-words sym-string (:dictionary-regex kb)))
        word-hyps
        (map (fn [sw-hyps]
               (let [word (apply str (map :subword sw-hyps))
                     pos-seq (mapcat :pos-seq sw-hyps)]
                 (new-hyp "Word" :word :word false conflicts
                          (/ (double (get (:unigram-model kb) [word]))
                             (double (:word-count kb)))
                          (concat sw-hyps
                                  (butlast (filter #(and (<= (:trans-pos %)
                                                             (last pos-seq))
                                                         (>= (:trans-pos %)
                                                             (first pos-seq)))
                                                   (sort-by :trans-pos merge-hyps))))
                          [] word
                          (format "Word: %s, pos-seq: %s" word
                                  (str/join ", " (map str pos-seq)))
                          {:pos-seq pos-seq :word word})))
             words)
        hyp-types (set (str/split (:HypTypes params) #","))]
    (concat merge-hyps split-hyps
            (if (hyp-types "words") word-hyps []))))
