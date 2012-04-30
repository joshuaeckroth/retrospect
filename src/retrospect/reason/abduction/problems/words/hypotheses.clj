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
                           (new-hyp "Sens" :sensor :symbol true nil 1.0 [] []
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
   
   (and (= :word (:type hyp1)) (= :word-transition (:type hyp2)))
   (and (< (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
        (> (last (:pos-seq hyp1)) (first (:pos-seq hyp2))))
   
   (and (= :word-transition (:type hyp1)) (= :word (:type hyp2)))
   (and (< (first (:pos-seq hyp2)) (first (:pos-seq hyp1)))
        (> (last (:pos-seq hyp2)) (first (:pos-seq hyp1))))
   
   (and (= :word (:type hyp1)) (= :in-word-transition (:type hyp2)))
   (or (= (first (:pos-seq hyp2)) (first (:pos-seq hyp1)))
       (= (dec (first (:pos-seq hyp2))) (last (:pos-seq hyp1))))
   
   (and (= :in-word-transition (:type hyp1)) (= :word (:type hyp2)))
   (or (= (first (:pos-seq hyp1)) (first (:pos-seq hyp2)))
       (= (dec (first (:pos-seq hyp1))) (last (:pos-seq hyp2))))

   (or (and (= :in-word-transition (:type hyp1)) (= :word-transition (:type hyp2)))
       (and (= :word-transition (:type hyp1)) (= :in-word-transition (:type hyp2))))
   (= (:explains hyp1) (:explains hyp2))

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
  similar-words (map first (find-substrings word (:dictionary-string kb)))
  similar-sum (reduce + (map (fn [w] (get (:unigram-model kb) [w]))
                             similar-words))
  )

(defn hypothesize
  [forced-hyps accepted hyps]
  (let [kb (get-kb hyps)
        sensor-hyps-sorted []
        choose-best (fn [hs h] (let [same-explains (filter #(= (:explains h) (:explains %))
                                                           hs)
                                     best? (= h (last (sort-by :apriori same-explains)))]
                                 (when best? h)))
        word-hyps []
        in-word-trans-hyps
        (filter identity
                (map (fn [hyp]
                       (when (has-edge? (:dtg kb) (:symbol1 hyp) (:symbol2 hyp))
                         (new-hyp "TransC" :in-word-transition :in-word-transition
                                  false conflicts
                                  ;; if this symbol pair is also a
                                  ;; word transition, figure out how
                                  ;; often it is vs. how often it is
                                  ;; an in-word transition; otherwise,
                                  ;; its score is 1.0
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
                         (new-hyp "TransW" :word-transition :word-transition
                                  false conflicts
                                  ;; if this symbol pair is also an
                                  ;; in-word transition, divide by how
                                  ;; often that is the case;
                                  ;; otherwise, its score is 1.0
                                  (let [w (get (:wtc kb) [(:symbol1 hyp) (:symbol2 hyp)])
                                        c (or (weight (:dtg kb)
                                                      (:symbol1 hyp) (:symbol2 hyp)) 0)]
                                    (/ (double w) (double (+ w c))))
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        start-of-word-hyps
        (filter identity
                (map (fn [hyp]
                       (when (has-edge? (:dtg kb) "start" (:symbol2 hyp))
                         ;; score is how often latter symbol is start
                         ;; of a word vs.  how often this symbol pair
                         ;; is an in-word transition; if the pair is
                         ;; never anything but a start->symbol2
                         ;; transition, its score is 1.0
                         (new-hyp "TransWS" :word-transition :word-transition
                                  false conflicts
                                  (let [w (weight (:dtg kb) "start" (:symbol2 hyp))
                                        c (or (weight (:dtg kb)
                                                      (:symbol1 hyp) (:symbol2 hyp)) 0)]
                                    (/ (double w) (double (+ w c))))
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        end-of-word-hyps
        (filter identity
                (map (fn [hyp]
                       (when (has-edge? (:dtg kb) (:symbol1 hyp) "end")
                         ;; score is how often former symbol is end of
                         ;; a word vs.  how often this symbol pair is
                         ;; an in-word transition; if the pair is
                         ;; never anything but a symbol1->end
                         ;; transition, its score is 1.0
                         (new-hyp "TransWE" :word-transition :word-transition
                                  false conflicts
                                  (let [w (weight (:dtg kb) (:symbol1 hyp) "end")
                                        c (or (weight (:dtg kb)
                                                      (:symbol1 hyp) (:symbol2 hyp)) 0)]
                                    (/ (double w) (double (+ w c))))
                                  [hyp] [] (str (:symbol1 hyp) (:symbol2 hyp)) ""
                                  {:pos-seq [(:pos2 hyp)]})))
                     sensor-hyps-sorted))
        all-word-trans-hyps (concat word-trans-hyps start-of-word-hyps end-of-word-hyps)
        best-word-trans-hyps (filter identity (map (partial choose-best all-word-trans-hyps)
                                                   all-word-trans-hyps))
        hyp-types (set (str/split (:HypTypes params) #","))]
    (concat
            (if (hyp-types "words") word-hyps [])
            (if (hyp-types "inwordtrans") in-word-trans-hyps [])
            (if (hyp-types "wordtrans") best-word-trans-hyps []))))
