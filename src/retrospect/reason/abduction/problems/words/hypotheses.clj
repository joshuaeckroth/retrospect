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
            {:pos pos :symbol symbol})])

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
               (assoc (read r) :max-word-length (apply max (map count training-dict))))
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

(defn same-word-hyp
  [pos-seq w h]
  (prof :same-word-hyp
        (and (= w (:word h))
             (= (:pos-seq h) pos-seq))))

(defn reset
  [])

(defn word-metrics
  [kb word]
  (prof :word-metrics
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
          (cond (= "mult" (:TendencyReduction params))
                (reduce * tendencies-no-nil)
                (= "opp" (:TendencyReduction params))
                (- 1.0 (reduce * (map #(- 1.0 %) tendencies-no-nil)))
                (= "avg" (:TendencyReduction params))
                (/ (reduce + tendencies-no-nil) (double (count tendencies-no-nil)))
                (= "min" (:TendencyReduction params))
                (apply min tendencies-no-nil)
                (= "max" (:TendencyReduction params))
                (apply max tendencies-no-nil)
                :else
                (reduce * tendencies-no-nil)))))

(defn score-learned-word
  [kb word]
  (prof :score-learned-word
        (let [avg-word-length (:avg-word-length kb)
              symbol-tendencies (:symbol-tendencies kb)
              tendency (word-metrics kb word)
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
          (/ pdf-true (+ pdf-true pdf-false)))))

(defn find-word
  [evidence left-sens right-sens kb]
  (prof :find-word
        (let [max-word-length (:max-word-length kb)
              unigram-model (get (:models kb) 1)
              sens-groups (mapcat (fn [left-n]
                                    (map (fn [right-n]
                                           (concat (take-last left-n left-sens)
                                                   [evidence]
                                                   (take right-n right-sens)))
                                         (range (- max-word-length left-n))))
                                  (range (dec max-word-length)))]
          (prof :find-word-filter
                (filter #(get unigram-model [(apply str (map :symbol %))]) sens-groups)))))

(defmethod hypothesize :sensor
  [evidence accepted rejected hyps]
  (prof :hyp-word
        (let [kb (get-kb hyps)
              sensor-hyps (sort-by :pos (get hyps :sensor))
              left-sens (filter #(< (:pos %) (:pos evidence)) sensor-hyps)
              right-sens (filter #(> (:pos %) (:pos evidence)) sensor-hyps)
              left-syms (apply str (map :symbol left-sens))
              right-syms (apply str (map :symbol right-sens))]
          (let [expls (find-word evidence left-sens right-sens kb)]
            (map (fn [expl]
                   (let [w (apply str (map :symbol expl))
                         tendency (word-metrics kb w)
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
                         similar-words (filter substring-filter (get substrings w))
                         similar-sum (reduce + (map (fn [w2] (get unigram-model [w2]))
                                                    similar-words))
                         prob (/ (double (get unigram-model [w])) (double similar-sum))]
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
                              expl [] w
                              (format "Word \"%s\" (pos %d-%d)\nTendency: %.2f"
                                      w (:pos (first expl)) (:pos (last expl)) tendency)
                              {:word w :pos-seq (map :pos expl) :tendency tendency})))
                 expls)))))

(defmethod hypothesize :word
  [evidence accepted rejected hyps]
  (prof :hyp-word-seq
        (let [kb (get-kb accepted)
              words-ordered (sort-by (comp first :pos-seq)
                                     (filter #(not= evidence %) (get accepted :word)))
              preceding (filter #(< (last (:pos-seq %))
                                    (first (:pos-seq evidence)))
                                words-ordered)
              preceding-nogaps (loop [pre (concat [evidence] (reverse preceding))
                                      nogaps []]
                                 (if (or (empty? pre) (nil? (second pre)))
                                   (reverse nogaps)
                                   (if (= (dec (first (:pos-seq (first pre))))
                                          (last (:pos-seq (second pre))))
                                     (recur (rest pre) (conj nogaps (second pre)))
                                     (reverse nogaps))))
              following (filter #(> (first (:pos-seq %))
                                    (last (:pos-seq evidence)))
                                words-ordered)
              following-nogaps (loop [fol (concat [evidence] following)
                                      nogaps []]
                                 (if (or (empty? fol) (nil? (second fol)))
                                   nogaps
                                   (if (= (last (:pos-seq (first fol)))
                                          (dec (first (:pos-seq (second fol)))))
                                     (recur (rest fol) (conj nogaps (second fol)))
                                     nogaps)))
              ngrams (:MaxModelGrams params)
              expl (concat (take-last (dec ngrams) preceding-nogaps)
                           [evidence]
                           (reverse (take-last (dec ngrams) (reverse following-nogaps))))
              word-seqs (filter #(> (count %) 1) (partition-all ngrams 1 expl))]
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
                  (recur (rest ws)))))))))

(defmulti learn
  (fn [evidence no-explainer-hyps hyps] (:type evidence)))

(defmethod learn :default [_ _ _] nil)

(defmethod learn :sensor
  [evidence noexp hyps]
  (prof :hyp-learn-word
        (let [sensor-noexp (filter #(= :sensor (:type %)) noexp)
              noexp-left (reverse (sort-by :pos (filter #(< (:pos %) (:pos evidence)) sensor-noexp)))
              noexp-right (sort-by :pos (filter #(> (:pos %) (:pos evidence)) sensor-noexp))
              left-pairs (take-while #(or (nil? (second %))
                                          (= (inc (:pos (first %))) (:pos (second %))))
                                     (partition-all 2 1 noexp-left))
              right-pairs (take-while #(or (nil? (second %))
                                           (= (inc (:pos (first %))) (:pos (second %))))
                                      (partition-all 2 1 noexp-right))
              left-hyps (reverse (concat (map first left-pairs)
                                         (if-let [h (second (last left-pairs))] [h] [])))
              right-hyps (concat (map first right-pairs)
                                 (if-let [h (second (last right-pairs))] [h] []))
              to-expl-hyps (concat (if (= (dec (:pos evidence)) (:pos (last left-hyps)))
                                     left-hyps [])
                                   [evidence]
                                   (if (= (inc (:pos evidence)) (:pos (first right-hyps)))
                                     right-hyps []))
              expl (sort-by :pos to-expl-hyps)
              word (apply str (map :symbol to-expl-hyps))
              kb (get-kb hyps)]
          (when (and (not-any? #(same-word-hyp (map :pos expl) word %) (get hyps :word))
                     (nil? (get (get (:models kb) 1) [word])))
            (let [pos-seq (map :pos expl)
                  tendency (word-metrics kb word)]
              [(new-hyp "LearnedWord" :word :learned-word false conflicts
                        (cond (= (:LearnApriori params) "tendency")
                              tendency
                              (= (:LearnApriori params) "gauss")
                              (score-learned-word kb word)
                              :else
                              tendency)
                        expl [] word
                        (format (str "Learned word: \"%s\" (pos %d-%d)\nTendency %.2f"
                                     "\nTo explain: %s")
                                word (first pos-seq) (last pos-seq) tendency
                                (str (:symbol evidence)))
                        {:word word :pos-seq pos-seq :tendency tendency})])))))

(defn get-left-hyps
  [evidence hyps]
  (prof :get-left-hyps
        (let [word-hyps (filter #(= :word (:type %)) hyps)
              left-hyps-all (reverse (sort-by (comp first :pos-seq)
                                              (filter #(< (last (:pos-seq %))
                                                          (first (:pos-seq evidence)))
                                                      word-hyps)))
              left-pairs (take-while #(or (nil? (second %))
                                          (= (dec (first (:pos-seq (first %))))
                                             (last (:pos-seq (second %)))))
                                     (partition-all 2 1 (concat [evidence] left-hyps-all)))
              left-hyps (reverse (rest (concat (map first left-pairs)
                                               (if-let [h (second (last left-pairs))] [h] []))))]
          (when (= (dec (first (:pos-seq evidence))) (last (:pos-seq (last left-hyps))))
            left-hyps))))

(defn get-right-hyps
  [evidence hyps]
  (prof :get-right-hyps
        (let [word-hyps (filter #(= :word (:type %)) hyps)
              right-hyps-all (sort-by (comp first :pos-seq)
                                      (filter #(> (first (:pos-seq %)) (last (:pos-seq evidence)))
                                              word-hyps))
              right-pairs (take-while #(or (nil? (second %))
                                           (= (inc (last (:pos-seq (first %))))
                                              (first (:pos-seq (second %)))))
                                      (partition-all 2 1 (concat [evidence] right-hyps-all)))
              right-hyps (rest (concat (map first right-pairs)
                                       (if-let [h (second (last right-pairs))] [h] [])))]
          (when (= (inc (last (:pos-seq evidence))) (first (:pos-seq (first right-hyps))))
            right-hyps))))

(defn expl-seqs
  [left evidence right]
  (prof :expl-seqs
        (letfn [(branch? [expl] (or (not-empty (:left (meta expl)))
                                    (not-empty (:right (meta expl)))))
                (children [expl]
                  (let [{:keys [left right]} (meta expl)]
                    (seq (filter identity [(when (not-empty left)
                                             (with-meta (vec (cons (last left) expl))
                                               {:left (butlast left) :right right}))
                                           (when (not-empty right)
                                             (with-meta (conj expl (first right))
                                               {:left left :right (rest right)}))]))))]
          (sort-by count (set (tree-seq branch? children
                                        (with-meta [evidence] {:left left :right right})))))))

(defmethod learn :word
  [evidence noexp hyps]
  (prof :hyp-learn-word-seq
        (let [left (take-last (:LearnWordLength params) (or (get-left-hyps evidence noexp) []))
              right (take (:LearnWordLength params) (or (get-right-hyps evidence noexp) []))
              kb (get-kb hyps)
              expls (expl-seqs left evidence right)]
          (prof :hlwseq-loop
                (loop [es expls
                       hs []]
                  (if (empty? es) (when (not-empty hs) hs)
                      (let [expl (first es)
                            word (apply str (map :word expl))]
                        (cond (prof :hlwseq-cond1
                                    (and (not-any? #(same-word-hyp (mapcat :pos-seq expl) word %)
                                                   (concat hs (get hyps :word)))
                                         (nil? (get (get (:models kb) 1) [word]))))
                              (let [tendency (word-metrics kb word)
                                    hyp (new-hyp "LWWord" :word :learned-word false conflicts
                                                 (cond (= (:LearnApriori params) "tendency")
                                                       tendency
                                                       (= (:LearnApriori params) "gauss")
                                                       (score-learned-word kb word)
                                                       :else
                                                       tendency)
                                                 (mapcat :explains expl) [] word
                                                 (format (str "Learned word from existing words: \"%s\" (pos %d-%d)"
                                                              "\nTendency: %.2f"
                                                              "\nTo explain: %s / Sequence: %s")
                                                         word (first (:pos-seq (first expl)))
                                                         (last (:pos-seq (last expl)))
                                                         tendency evidence (str/join ", " expl))
                                                 {:word word :pos-seq (mapcat :pos-seq expl)
                                                  :tendency tendency})]
                                (recur (rest es) (conj hs hyp)))
                              :else (recur (rest es) hs)))))))))
