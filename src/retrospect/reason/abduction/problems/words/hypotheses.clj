(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
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
            {:pos pos :pos-seq [pos] :symbol symbol})])

(defn conflicts
  "Two words-domain hypotheses conflict if: (1) they are both composite
   hypotheses and it is not the case that the tail of one is the prefix of the
   other; or (2) they overlap in their start-end range."
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

(defn build-symbol-tendencies
  [training]
  (letfn [(inc-sym [m sym pos] (assoc-in m [pos sym] (inc (get (pos m) sym 0))))]
    (reduce (fn [m sentence]
              (reduce (fn [m2 w]
                        (reduce (fn [m3 i]
                                  (let [sym (nth w i)
                                        m3-occur (inc-sym m3 sym :occur)]
                                    (cond (= i 0)
                                          (inc-sym m3-occur sym :front)
                                          (= i (dec (count w)))
                                          (inc-sym m3-occur sym :back)
                                          :else
                                          (inc-sym m3-occur sym :middle))))
                                m2 (range 0 (count w))))
                      m sentence))
            {} training)))

(defn generate-kb
  [[training training-dict]]
  (let [kb (try
             (with-open [r (java.io.PushbackReader.
                            (clojure.java.io/reader
                             (format "%s/words/kb-%s-%d.clj" @datadir
                                     (:Dataset params) (:MaxModelGrams params))))]
               (read r))
             (catch Exception _
               (let [models (build-markov-models training)
                     model-sums (reduce (fn [m-s n]
                                          (assoc m-s n (double (reduce + (vals (get models n))))))
                                        {} (keys models))
                     substrings (reduce (fn [m w] (assoc m w (filter #(substring? w %) training-dict)))
                                        {} training-dict)
                     symbol-words (reduce (fn [m w] (reduce (fn [m2 sym]
                                                              (if (nil? (get m2 sym))
                                                                (assoc m2 sym #{w})
                                                                (update-in m2 [sym] conj w)))
                                                            m (seq w)))
                                          {} training-dict)
                     symbol-tendencies (build-symbol-tendencies training)
                     dict-counts (map count training-dict)
                     kb {:avg-word-length (if (empty? training-dict) 0
                                              (double (/ (reduce + dict-counts)
                                                         (count training-dict))))
                         :max-word-length (apply max dict-counts)
                         :dictionary training-dict
                         :models models
                         :model-sums model-sums
                         :symbol-words symbol-words
                         :symbol-tendencies symbol-tendencies
                         :substrings substrings}]
                 (spit (format "%s/words/kb-%s-%d.clj" @datadir
                               (:Dataset params) (:MaxModelGrams params))
                       (pr-str kb))
                 kb)))]
    [(new-hyp "KB" :kb :kb false conflicts 1.0 [] [] "" "" kb)]))

(defn get-kb
  [hyps]
  (first (get hyps :kb)))

(defmulti hypothesize
  (fn [evidence accepted rejected hyps] (:type evidence)))

(defmethod hypothesize :default [_ _ _ _] nil)

(defn word-metrics
  [kb word]
  (let [symbol-tendencies (:symbol-tendencies kb)
        tendencies (map (fn [i]
                          (let [sym (nth word i)
                                get-pos (fn [pos] (double (get (pos symbol-tendencies) sym 0.0)))]
                            (if-let [occur (get (:occur symbol-tendencies) sym)]
                              (cond (= i 0)
                                    (/ (get-pos :front) (double occur))
                                    (= i (dec (count word)))
                                    (/ (get-pos :back) (double occur))
                                    :else
                                    (/ (get-pos :middle) (double occur))))))
                        (range (count word)))
        tendencies-no-nil (let [ts (filter identity tendencies)]
                            (if (empty? ts) [0.0] ts))]
    {:mult (reduce * tendencies-no-nil)
     :opp (- 1.0 (reduce * (map #(- 1.0 %) tendencies-no-nil)))
     :avg (/ (reduce + tendencies-no-nil) (double (count tendencies-no-nil)))
     :min (apply min tendencies-no-nil)
     :max (apply max tendencies-no-nil)}))

(defn select-tendency
  [wm]
  (get wm (keyword (:TendencyReduction params)) (:mult wm)))

(defn score-learned-word
  [kb word]
  (let [avg-word-length (:avg-word-length kb)
        symbol-tendencies (:symbol-tendencies kb)
        tendency (:mult (word-metrics kb word))
        length-diff (Math/abs (double (- avg-word-length (count word))))
        length-diff-pct (/ length-diff avg-word-length)
        calc (/ (- tendency length-diff-pct) (+ tendency length-diff-pct))
        ;; in R:
        ;;   library(MASS)
        ;;   fitdistr(subset(d, V0 > 0), "normal")
        mean-true 0.238622170
        sd-true 0.343971720
        mean-false -0.4736185783
        sd-false 0.4623790242
        pdf-true (* (/ 1.0 (* sd-true (Math/sqrt (* 2 Math/PI))))
                    (Math/exp (- (/ (Math/pow (- calc mean-true) 2.0)
                                    (* 2.0 sd-true sd-true)))))
        pdf-false (* (/ 1.0 (* sd-false (Math/sqrt (* 2 Math/PI))))
                     (Math/exp (- (/ (Math/pow (- calc mean-false) 2.0)
                                     (* 2.0 sd-false sd-false)))))]
    (/ pdf-true (+ pdf-true pdf-false))))

(defn get-left-hyps
  [evidence hyps]
  (let [left-hyps-all (reverse (sort-by (comp first :pos-seq)
                                        (filter #(< (last (:pos-seq %))
                                                    (first (:pos-seq evidence)))
                                                hyps)))
        left-pairs (take-while #(or (nil? (second %))
                                    (= (dec (first (:pos-seq (first %))))
                                       (last (:pos-seq (second %)))))
                               (partition-all 2 1 (concat [evidence] left-hyps-all)))
        left-hyps (reverse (rest (concat (map first left-pairs)
                                         (if-let [h (second (last left-pairs))] [h] []))))]
    (when (= (dec (first (:pos-seq evidence))) (last (:pos-seq (last left-hyps))))
      left-hyps)))

(defn get-right-hyps
  [evidence hyps]
  (let [right-hyps-all (sort-by (comp first :pos-seq)
                                (filter #(> (first (:pos-seq %))
                                            (last (:pos-seq evidence)))
                                        hyps))
        right-pairs (take-while #(or (nil? (second %))
                                     (= (inc (last (:pos-seq (first %))))
                                        (first (:pos-seq (second %)))))
                                (partition-all 2 1 (concat [evidence] right-hyps-all)))
        right-hyps (rest (concat (map first right-pairs)
                                 (if-let [h (second (last right-pairs))] [h] [])))]
    (when (= (inc (last (:pos-seq evidence))) (first (:pos-seq (first right-hyps))))
      right-hyps)))

(defn gen-seqs-around
  [middle hyps max-length]
  (let [left (get-left-hyps (first middle) hyps)
        right (get-right-hyps (last middle) hyps)]
    (set (mapcat (fn [left-n]
                   (map (fn [right-n]
                          (concat (take-last left-n left)
                                  middle
                                  (take right-n right)))
                        (range (inc (- max-length left-n (count middle))))))
                 (range (- max-length (count middle)))))))

(defmethod hypothesize :sensor
  [evidence accepted rejected hyps]
  (let [kb (get-kb hyps)
        unigram-model (get (:models kb) 1)
        bigram-model (get (:models kb) 2)
        sensor-hyps (sort-by :pos (get hyps :sensor))
        left-sens (filter #(< (:pos %) (:pos evidence)) sensor-hyps)
        right-sens (filter #(> (:pos %) (:pos evidence)) sensor-hyps)
        left-syms (apply str (map :symbol left-sens))
        right-syms (apply str (map :symbol right-sens))
        seqs (gen-seqs-around [evidence] sensor-hyps (:max-word-length kb))
        seqs-words (filter #(get unigram-model [(apply str (map :symbol %))]) seqs)]
    (let [expls seqs-words]
      (map (fn [expl]
             (let [word (apply str (map :symbol expl))
                   tendencies-map (word-metrics kb word)
                   tendency (select-tendency tendencies-map)
                   gauss (score-learned-word kb word)
                   unigram-model (get (:models kb) 1)
                   substrings (:substrings kb)
                   substring-filter (cond (= (:WordContext params) "data")
                                          #(let [syms (concat (take-last (dec (count %)) left-syms)
                                                              [(:symbol evidence)]
                                                              (take (dec (count %)) right-syms))]
                                             (substring? % (apply str syms)))
                                          (= (:WordContext params) "global")
                                          (constantly true)
                                          :else
                                          (constantly true))
                   similar-words (filter substring-filter (get substrings word))
                   similar-sum (reduce + (map (fn [w] (get unigram-model [w]))
                                              similar-words))
                   prob (/ (double (get unigram-model [word])) (double similar-sum))]
               (new-hyp "Word" :word :word true conflicts
                        (cond (= "tendency" (:WordApriori params))
                              tendency
                              (= "prob" (:WordApriori params))
                              prob
                              (= "mult" (:WordApriori params))
                              (* tendency prob)
                              (= "max" (:WordApriori params))
                              (max tendency prob)
                              (= "min" (:WordApriori params))
                              (min tendency prob)
                              (= "avg" (:WordApriori params))
                              (/ (+ tendency prob) 2.0)
                              :else prob)
                        expl [] word
                        (format "Word \"%s\" (pos %d-%d)\nTendency: %.2f"
                                word (:pos (first expl)) (:pos (last expl)) tendency)
                        {:word word :pos-seq (map :pos expl)
                         :unigram-freq (get unigram-model [word])
                         :bigram-freq 0 #_(reduce + (map (fn [k] (get bigram-model k))
                                                         (filter (fn [[w1 w2]] (or (= w1 word)
                                                                                   (= w2 word)))
                                                                 (keys bigram-model))))
                         :similar-sum similar-sum
                         :probData prob
                         :probGlobal (/ (double (get unigram-model [word]))
                                        (reduce + (map (fn [w] (get unigram-model [w]))
                                                       (get substrings word))))
                         :tendencies-map tendencies-map :gauss gauss})))
           expls))))

(defmethod hypothesize :word
  [evidence accepted rejected hyps]
  (let [kb (get-kb accepted)
        word-hyps (filter #(not= evidence %) (get accepted :word))
        ngrams (:MaxModelGrams params)
        expls (gen-seqs-around [evidence] word-hyps ngrams)
        word-seqs (filter second expls)]
    (loop [ws word-seqs]
      (when (not-empty ws)
        (let [word-seq (first ws)
              model (get (:models kb) (count word-seq))
              words (map :word word-seq)
              freq (get model words)
              prob (if freq (/ (double freq) (get (:model-sums kb) (count word-seq))))]
          (if (and freq (>= (count word-seq) 2))
            (let [pos-seqs (map :pos-seq word-seq)
                  hyp (new-hyp "WordSeq" :word-seq :word-seq false conflicts
                               prob word-seq [] (str/join " " words)
                               (format "WordSeq \"%s\" (pos %d-%d)\nFreq: %d\nProb: %f"
                                       (str/join " " words)
                                       (ffirst pos-seqs) (last (last pos-seqs))
                                       freq prob)
                               {:words words :pos-seqs pos-seqs})]
              (if (not-any? (fn [h] (hyps-equal? hyp h)) (get hyps :word-seq))
                [hyp]
                (recur (rest ws))))
            (recur (rest ws))))))))

(defn valid-learn-hyp
  [kb hyps expl]
  (let [pos-seq (mapcat :pos-seq expl)
        word (apply str (map :word expl))]
    (and (not-any? (fn [h] (and (= word (:word h)) (= pos-seq (:pos-seq h))))
                   (get hyps :word))
         (nil? (get (get (:models kb) 1) [word])))))

(defn learn
  [evidence to-expl hyps]
  (let [kb (get-kb hyps)
        to-expl-hyps (filter #(= :sensor (:type %)) to-expl)
        expl-seqs (gen-seqs-around
                   (if (= :word (:type evidence)) (:explains evidence) [evidence])
                   to-expl-hyps (:LearnWordLength params))
        expls (filter (partial valid-learn-hyp kb hyps) expl-seqs)]
    (map (fn [expl]
           (let [word (apply str (map :symbol expl))
                 tendencies-map (word-metrics kb word)
                 gauss (score-learned-word kb word)]
             (new-hyp "LearnedWord"
                      :word :learned-word false conflicts
                      (cond (= (:LearnApriori params) "tendency")
                            (select-tendency tendencies-map)
                            (= (:LearnApriori params) "gauss")
                            gauss
                            :else
                            (select-tendency tendencies-map))
                      expl [] word
                      (format (str "Learned word: \"%s\" (pos %d-%d)\nTendency: %.2f\nGauss: %.2f"
                                   "\nTo explain: %s / Sequence: %s")
                              word (first (:pos-seq (first expl)))
                              (last (:pos-seq (last expl)))
                              (select-tendency tendencies-map) gauss
                              evidence (str/join ", " expl))
                      {:word word :pos-seq (mapcat :pos-seq expl)
                       :tendencies-map tendencies-map :gauss gauss})))
         expls)))
