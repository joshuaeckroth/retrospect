(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.reason.abduction.problems.words.evaluate :only [hyps-equal?]])
  (:use [retrospect.problems.words.learning :only
         [update-features calc-centroid similarity]])
  (:use [retrospect.problems.words.symbols])
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

(comment
  (defn count-changes
    [word letters]
    (reduce (fn [c i] (if (not= (nth word i) (nth letters i)) (inc c) c))
            0 (range (count word)))))

(comment
  (defn make-word-hyp
    [word pos-seq letters noise? left-off sensor-hyps models]
    (let [explains (map #(nth sensor-hyps %) pos-seq)
          adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))
          unimodel (get models 1)
          changes (count-changes word letters)
          ;; heuristic
          changes-factor (/ (- (count word) changes) (count word))
          apriori (max 0.001 (* prob (Math/pow changes-factor 3.0)))]
      (new-hyp (if noise? "WordNoisy" "Word") :word
               (if noise? :noise-word :word) conflicts?
               apriori :and explains []
               (format "Word: \"%s\" at positions %s (%s) (%d changes)"
                       word (str adjusted-pos-seq) (apply str letters) changes)
               {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
                :words [word] :pos-seqs [adjusted-pos-seq] :changes changes}))))

(defn build-markov-models
  "Build a Markov n-gram model of word transitions."
  [training]
  (reduce (fn [models sentence]
            (let [words (filter #(not (re-matches punctuation-regex %)) sentence)
                  words-grouped (apply concat (for [i (range 1 (inc (:MaxModelGrams params)))]
                                                (partition i (concat (repeat (dec i) "") words))))]
              (reduce (fn [ms ws] (let [m (get ms (count ws) {})
                                        prior (get m ws 0)]
                                    (assoc-in ms [(count ws) ws] (inc prior))))
                      models words-grouped)))
          {} (loop [sentences training
                    word-seqs []]
               (if (empty? sentences) word-seqs
                   (let [sentence (first sentences)
                         word-seq (take-while #(not (re-matches punctuation-regex %))
                                              sentence)
                         remaining (take-last (dec (- (count sentence) (count word-seq)))
                                              sentence)]
                     (recur (if (empty? remaining) (rest sentences)
                                (conj (rest sentences) remaining))
                            (if (empty? word-seq) word-seqs
                                (conj word-seqs word-seq))))))))

(defn generate-kb
  [[training training-dict]]
  (let [models (build-markov-models training)
        features (update-features {} training-dict)]
    [(new-hyp "KB" :kb :kb false conflicts 1.0 [] [] "" ""
              {:avg-word-length (if (empty? training-dict) 0
                                    (double (/ (reduce + (map count training-dict))
                                               (count training-dict))))
               :dictionary training-dict
               :models models
               :features features
               :centroid (calc-centroid features (get models 1))})]))

(defn get-kb
  [hyps]
  (first (get hyps :kb)))

(defmulti hypothesize
  (fn [evidence accepted rejected hyps] (:type evidence)))

(defmethod hypothesize :default [_ _ _ _] nil)

(defn same-word-hyp
  [pos-start w h]
  (and (= w (:word h))
       (= (first (:pos-seq h)) pos-start)))

(def cache (ref nil))

(defn reset
  [] (dosync (alter cache (constantly nil))))

(defn update-cache
  [hyps]
  (let [kb (get-kb hyps)
        sensor-hyps (vec (sort-by :pos (get hyps :sensor)))
        sensor-str (apply str (map :symbol sensor-hyps))
        words (map first (keys (get (:models kb) 1)))
        word-positions (mapcat (fn [w]
                                 (let [matcher (re-matcher
                                                (re-pattern (format "(%s)" (Pattern/quote w)))
                                                sensor-str)]
                                   (loop [matches []]
                                     (if (re-find matcher)
                                       (recur (conj matches
                                                    (subvec sensor-hyps (.start matcher 1)
                                                            (+ (.start matcher 1) (count w)))))
                                       matches))))
                               words)]
    (dosync (alter cache (constantly word-positions)))))

(defn find-word
  [evidence other-hyps]
  (let [matches-near-evidence (filter #(and (<= (:pos (first %)) (:pos evidence))
                                            (>= (:pos (last %)) (:pos evidence)))
                                      @cache)
        nearby-sensor-hyps (reverse (sort-by count matches-near-evidence))]
    (filter (fn [sensor-hyps] (not-any? #(same-word-hyp (:pos (first sensor-hyps))
                                                   (apply str (map :symbol sensor-hyps)) %)
                                        other-hyps))
            nearby-sensor-hyps)))

(defmethod hypothesize :sensor
  [evidence accepted rejected hyps]
  (when (nil? @cache) (update-cache hyps))
  (let [sensor-hyps (get accepted :sensor)
        other-hyps (concat (get hyps :word) (get hyps :word-seq))]
    (if (re-matches punctuation-regex (str (:symbol evidence)))
      [(new-hyp "Punc" :punctuation :punctuation false nil 1.0 [evidence] []
                (str (:symbol evidence)) (format "Punctuation: %s" (str (:symbol evidence)))
                {:pos (:pos evidence) :symbol (:symbol evidence)})
       nil]
      (let [[expl & word-sensor-hyps] (find-word evidence other-hyps)]
        (if expl
          (let [w (apply str (map :symbol expl))
                unigram-model (get (:models (get-kb hyps)) 1)
                similar-words (filter #(substring? w %) (map first (keys unigram-model)))
                similar-sum (reduce + (map (fn [w2] (get unigram-model [w2]))
                                           similar-words))]
            [(new-hyp "Word" :word :word true conflicts
                      (double (/ (get unigram-model [w]) similar-sum))
                      expl [] w
                      (format "Word \"%s\" (pos %d-%d)"
                              w (:pos (first expl))
                              (:pos (last expl)))
                      {:word w :pos-seq (map :pos expl)})
             ;; what's the estimate of "more" hyps?
             0.5]))))))

(defmethod hypothesize :word
  [evidence accepted rejected hyps]
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
              freq (get model words)]
          (if (and freq (>= (count word-seq) 2))
            (let [pos-seqs (map :pos-seq word-seq)
                  hyp (new-hyp "WordSeq" :word-seq :word-seq false conflicts
                               0.5 word-seq [] (str/join " " words)
                               (format "WordSeq \"%s\" (pos %d-%d)"
                                       (str/join " " words)
                                       (ffirst pos-seqs) (last (last pos-seqs)))
                               {:words words :pos-seqs pos-seqs})]
              (if (not-any? (fn [h] (hyps-equal? hyp h)) (get hyps :word-seq))
                [hyp nil]
                (recur (rest ws))))
            (recur (rest ws))))))))

(defmulti learn
  (fn [evidence no-explainer-hyps hyps] (:type evidence)))

(defmethod learn :default [_ _ _] nil)

(defmethod learn :sensor
  [evidence noexp hyps]
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
        kb (get-kb hyps)
        avg-word-length (:avg-word-length kb)
        centroid (:centroid kb)]
    (when (and (not-any? #(same-word-hyp (:pos (first expl)) word %) (get hyps :word))
               (< (Math/abs (- avg-word-length (count word))) avg-word-length))
      (let [pos-seq (map :pos expl)
            sim (Math/log (+ 1 (* (:SimMultiplier params)
                                  (similarity word centroid))))
            length-diff (Math/abs (- avg-word-length (count word)))
            apriori (min 1.0 (+ sim (Math/pow (:LengthPenalty params)
                                              (inc length-diff))))]
        [(new-hyp "LearnedWord" :word :learned-word true conflicts
                  apriori expl [] word
                  (format (str "Learned word: \"%s\" (pos %d-%d)"
                               "\nTo explain: %s"
                               "\nSimilarity measure: %.2f")
                          word (first pos-seq) (last pos-seq)
                          (str (:symbol evidence)) sim)
                  {:word word :pos-seq pos-seq})]))))

(defn get-left-hyps
  [evidence hyps]
  (let [word-hyps (filter #(= :word (:type %)) hyps)
        left-hyps-all (reverse (sort-by (comp first :pos-seq)
                                        (filter #(< (last (:pos-seq %)) (first (:pos-seq evidence)))
                                                word-hyps)))
        left-pairs (take-while #(or (nil? (second %))
                                    (= (dec (first (:pos-seq (first %)))) (last (:pos-seq (second %)))))
                               (partition-all 2 1 left-hyps-all))
        left-hyps (reverse (concat (map first left-pairs)
                                   (if-let [h (second (last left-pairs))] [h] [])))]
    (when (= (dec (first (:pos-seq evidence))) (last (:pos-seq (last left-hyps))))
      left-hyps)))

(defn get-right-hyps
  [evidence hyps]
  (let [word-hyps (filter #(= :word (:type %)) hyps)
        right-hyps-all (sort-by (comp first :pos-seq)
                                (filter #(> (first (:pos-seq %)) (last (:pos-seq evidence)))
                                        word-hyps))
        right-pairs (take-while #(or (nil? (second %))
                                     (= (inc (last (:pos-seq (first %)))) (first (:pos-seq (second %)))))
                                (partition-all 2 1 right-hyps-all))
        right-hyps (concat (map first right-pairs)
                           (if-let [h (second (last right-pairs))] [h] []))]
    (when (= (inc (last (:pos-seq evidence))) (first (:pos-seq (first right-hyps))))
      right-hyps)))

(defn expl-seqs
  [left evidence right]
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
                                  (with-meta [evidence] {:left left :right right}))))))

(defmethod learn :word
  [evidence noexp hyps]
  (let [left (or (get-left-hyps evidence noexp)
                 (get-left-hyps evidence (get hyps :word))
                 [])
        right (or (get-right-hyps evidence noexp)
                  (get-right-hyps evidence (get hyps :word))
                  [])
        kb (get-kb hyps)
        centroid (:centroid kb)
        avg-word-length (:avg-word-length kb)
        expls (filter #(> (* 2 avg-word-length) (count (apply str (map :word %))))
                      (expl-seqs left evidence right))]
    (loop [es expls
           hs []]
      (if (empty? es) (when (not-empty hs) hs)
          (let [expl (first es)
                word (apply str (map :word expl))]
            (cond (not-any? #(same-word-hyp (first (:pos-seq (first expl))) word %)
                            (get hyps :word))
                  (let [sim (Math/log (+ 1 (* (:SimMultiplier params)
                                              (similarity word centroid))))
                        length-diff (Math/abs (- avg-word-length (count word)))
                        apriori (min 1.0 (+ sim (Math/pow (:LengthPenalty params)
                                                          (inc length-diff))))
                        hyp (new-hyp "LWord" :word :learned-word true conflicts
                                     apriori (mapcat :explains expl) [] word
                                     (format (str "Learned word from existing words: \"%s\" (pos %d-%d)"
                                                  "\nTo explain: %s\nEntire sequence: %s"
                                                  "\nSimilarity measure: %.2f")
                                             word (first (:pos-seq (first expl)))
                                             (last (:pos-seq (last expl)))
                                             evidence (str/join ", " expl) sim)
                                     {:word word :pos-seq (mapcat :pos-seq expl)})]
                    (recur (rest es) (conj hs hyp)))
                  (and (nil? (get (get (:models kb) (count expl)) (map :word expl)))
                       (not-any? (fn [h] (hyps-equal? h {:type :word-seq
                                                         :pos-seqs (map :pos-seq expl)
                                                         :words (map :word expl)}))
                                 (get hyps :word-seq)))
                  (let [hyp (new-hyp "LWordSeq" :word-seq :learned-word-seq false conflicts
                                     0.0 expl [] (str/join " " (map :word expl))
                                     (format (str "Learned word sequence: \"%s\" (pos %d-%d)"
                                                  "\nTo explain: %s\nEntire sequence: %s")
                                             (str/join " " (map :word expl))
                                             (first (:pos-seq (first expl)))
                                             (last (:pos-seq (last expl)))
                                             evidence (str/join "; " expl))
                                     {:words (map :word expl) :pos-seqs (map :pos-seq expl)})]
                    (recur (rest es) (conj hs hyp)))
                  :else (recur (rest es) hs)))))))

(comment
  (defn make-learned-word-hyp
    [word pos-seq letters left-off sensor-hyps avg-word-length centroid]
    (let [explains (map #(nth sensor-hyps %) pos-seq)
          adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))
          
]
      (new-hyp "WordLearn" :word :learned-word conflicts?
               apriori :and explains []
               (format "Learned word: \"%s\" at positions %s (%s) (sim: %.4f)"
                       word (str adjusted-pos-seq) (apply str letters) sim)
               {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
                :words [word] :pos-seqs [adjusted-pos-seq] :sim sim}))))

(comment
  (defn acceptable-noise?
    [letters word noise?]
    (let [max-noise (if-not noise? 0
                            (Math/ceil
                             (* (count word)
                                (min 1.0 (+ (/ (:SensorNoise params) 100.0)
                                            (/ (- 1.0 (/ (:BelievedKnowledge params) 100.0))
                                               4.0))))))]
      (let [w1 (to-array letters)
            w2 (to-array word)]
        (>= max-noise (areduce w1 i c 0 (if (not= (aget w1 i) (aget w2 i)) (inc c) c)))))))

(comment
  (defn make-starts-ends
    [hyps]
    (map (fn [h] [(:start (:data h)) (:end (:data h))]) hyps)))

(comment
  (defn gap-sizes
    [starts-ends]
    (map #(- (second %) (first %))
         (partition 2 (interleave (butlast (map second starts-ends))
                                  (rest (map first starts-ends)))))))

(comment
  (defn search-word
    "Find all possible occurrences (including jumps forward) of word in
   parts (partitions of the letters having same length as word). Each
   possible occurrence is a vector of positions (which are in
   increasing order, representing letter positions). Allow a maximum
   of max-noise letters to differ from word."
    [word parts noise?]
    (filter (fn [[_ letters]] (acceptable-noise? word letters noise?))
            (map (fn [lps] [(map first lps) (map second lps)]) parts))))

(comment
  (defn search-dict-words
    [indexed-letters dict noise?]
    ;; pre-compute all possible partitions (for all word lengths)
    (let [max-word-length (apply max 0 (map count dict))
          filter-gaps (fn [i-ls]
                        ;; create a starts-ends list like ([1 1] [2 2] ...)
                        (filter #(let [starts-ends (map (fn [i] [i i]) (map first %))]
                                   (= 1 (apply max 1 (gap-sizes starts-ends)))) i-ls))
          parts (map (fn [length] (filter-gaps (partition length 1 indexed-letters)))
                     (range (:MinWordLength params) (inc max-word-length)))]
      (reduce (fn [m w] (assoc m w (search-word w (nth parts (- (count w)
                                                                (:MinWordLength params)))
                                                noise?)))
              {} dict))))

(comment
  (defn make-word-hyps
    [indexed-letters left-off dict noise? sensor-hyps models]
    (let [words (reduce (fn [m w] (if (not-empty (m w)) m (dissoc m w)))
                        (search-dict-words indexed-letters dict noise?) dict)]
      (filter :apriori
              (for [word (keys words) [pos-seq letters] (get words word)]
                (make-word-hyp word pos-seq letters noise? left-off sensor-hyps models))))))

(comment
  (defn valid-composite?
    [hyps]
    (and (every? #(= 1 %) (gap-sizes (make-starts-ends hyps)))
         (not-any? (fn [[h1 h2]] (conflicts? h1 h2)) (partition 2 1 hyps)))))

(comment
  (defn gen-valid-composites
    [word-hyps accepted max-n]
    (let [accepted-sorted (sort-by (comp :start :data) accepted)
          composites (map #(sort-by (comp :start :data) %)
                          (mapcat (fn [n]   
                                    (combinations
                                     (sort-by (comp :start :data)
                                              (concat (take-last (dec n) accepted-sorted)
                                                      word-hyps))
                                     n))
                                  (range 2 (inc max-n))))]
      (set (filter valid-composite? (filter (fn [c] (some #(not (accepted %)) c))
                                            composites))))))

(comment
  (defn lookup-prob
    [models word-hyps accepted]
    (let [acc-indexes (loop [indexes []
                             i 0
                             ws word-hyps]
                        (if (empty? ws) indexes
                            (let [c (count (:words (:data (first ws))))]
                              (recur 
                               (if (accepted (first ws))
                                 (concat indexes (range i (+ i c)))
                                 indexes)
                               (+ i c) (rest ws)))))
          words (vec (mapcat (comp :words :data) word-hyps))
          model (get models (count words))
          alt-ngrams (if (empty? acc-indexes)
                       (filter (fn [ws] (= (butlast ws) (butlast words))) (keys model))
                       (filter (fn [ws] (every? (fn [i] (= (nth ws i) (nth words i)))
                                                acc-indexes))
                               (keys model)))
          sum (reduce + (vals (select-keys model alt-ngrams)))
          c (get model words)]
      ;; if we have a probability in the model, return it
      (if c (max 0.001 (double (/ c (if (= 0 sum) (:sum (meta model)) sum))))))))

(comment
  (defn make-composite-hyps
    [models word-hyps accepted max-n]
    (let [composites (gen-valid-composites word-hyps accepted max-n)]
      (filter :apriori
              (for [c composites]
                (let [words (mapcat (comp :words :data) c)
                      pos-seqs (mapcat (comp :pos-seqs :data) c)
                      c-aprioris (map :apriori c)
                      c-set (set c)
                      accepted-words (set/intersection accepted c-set)
                      unaccepted (set/difference c-set accepted)
                      depends (set (for [h accepted-words]
                                     (if-let [com (first (filter
                                                          (fn [c] (some #{h} (:explains c)))
                                                          (filter #(= :word-seq (:type %))
                                                                  accepted)))]
                                       com h)))
                      apriori (lookup-prob models c accepted-words)]
                  (new-hyp "WordSeq" :word-seq :word-seq conflicts?
                           (if apriori (* apriori (apply min c-aprioris)))
                           :and unaccepted depends ;; explains & depends
                           (format "Word sequence \"%s\" at positions %s"
                                   (apply str (interpose " " words))
                                   (apply str (interpose ", " pos-seqs)))
                           {:start (:start (:data (first c))) :end (:end (:data (last c)))
                            :pos-seqs pos-seqs :words words})))))))

(comment
  (defn inconsistent
    [pdata hyps rejected]
    []))

(comment
  (defn make-indexed-letters
    [letters]
    ;; if not enough sensed data to fill time, indexed-letters will have some
    ;; non-characters; so filter those out
    (filter #(= java.lang.Character (type (second %)))
            (map (fn [i] [i (nth letters i)]) (range (count letters))))))

(comment
  (defn hypothesize
    [ep-state sensors time-now]
    (binding [compute 0 memory 0]
      (let [sens (first sensors) ;; only one sensor
            {:keys [dictionary left-off models]} (:problem-data ep-state)
            accepted (:accepted (:problem-data ep-state))
            max-n (apply max (keys models))
            sensor-hyps (make-sensor-hyps indexed-letters)
            ep-sensor-hyps (reduce #(add-fact %1 %2) ep-state sensor-hyps)
            ep-letters (assoc-in ep-sensor-hyps [:problem-data :indexed-letters]
                                 indexed-letters)
            word-hyps (make-word-hyps indexed-letters left-off dictionary false
                                      sensor-hyps models)
            composite-hyps (make-composite-hyps models word-hyps accepted max-n)]
        [(reduce (fn [ep hyp] (add-hyp ep hyp))
                 ep-letters (concat word-hyps composite-hyps))
         {:compute compute :memory memory}]))))

(comment
  (defn make-sensor-noise-hyps
    [sub-indexed-letters left-off dictionary max-n sensor-hyps accepted models]
    (make-word-hyps sub-indexed-letters left-off
                    dictionary true sensor-hyps models)))

(comment
  (defn make-learning-hyps
    [indexed-letters unexp-pos left-off dictionary sensor-hyps
     avg-word-length centroid last-time word-hyps]
    ;; some heuristics:
    ;; don't learn words that stop at or later than
    ;; 3 letters from the time boundary, or
    ;; whose 'sim' value is less than 1.0, or
    ;; do not fit the full length between some two word hyps
    ;; (if any word hyp proceeds or follows)
    (let [contig-subsets (loop [ps (rest unexp-pos)
                                subs (if-let [p (first unexp-pos)] [[p]] [])]
                           (cond (empty? ps) subs
                                 (= (first ps) (inc (last (last subs))))
                                 (recur (rest ps) (conj (vec (butlast subs))
                                                        (conj (vec (last subs))
                                                              (first ps))))
                                 :else (recur (rest ps) (conj subs [(first ps)]))))
          contig-subsets-allsizes (mapcat (fn [sub]
                                            (mapcat #(partition % 1 sub)
                                                    (range (:MinLearnLength params)
                                                           (inc (:MaxLearnLength params)))))
                                          contig-subsets)
          words (sort-by first (map (fn [subset] (map (fn [i] (nth indexed-letters i))
                                                      subset)) contig-subsets-allsizes))
          ;; learn only words that are not in the dictionary
          new-words (filter (fn [w] (not (dictionary (apply str (map second w))))) words)
          snug? (fn [hyps h]
                  (let [w-hyps-prior (filter #(< (:end (:data %)) (:start (:data h))) hyps)
                        w-hyps-after (filter #(> (:start (:data %)) (:end (:data h))) hyps)]
                    (and (or (empty? w-hyps-prior)
                             (some #(= (inc (:end (:data %))) (:start (:data h)))
                                   w-hyps-prior))
                         (or (empty? w-hyps-after)
                             (some #(= (dec (:start (:data %))) (:end (:data h)))
                                   w-hyps-after)))))
          learning-hyps (map (fn [w] (make-learned-word-hyp
                                      (apply str (map second w))
                                      (map first w) (map second w)
                                      left-off sensor-hyps avg-word-length centroid))
                             (filter #(< (first (last %)) (- last-time 3)) new-words))]
      (filter (fn [h] (snug? (concat word-hyps learning-hyps) h)) learning-hyps))))

(comment
  (defn find-adjacent-hyps
    [unexp-pos word-hyps]
    (filter (fn [h] (let [s (:start (:data h))
                          e (:end (:data h))]
                      (not-empty
                       (set/intersection
                        unexp-pos (set [(inc s) (dec s)
                                        (inc e) (dec e)])))))
            word-hyps)))

(comment
  (defn get-more-hyps
    "The `(hypothesize)` function only offers word hypotheses that have
   no noise correction (no replaced characters; so the words must
   match identically with some part of the input stream). However,
   there may actually be noise in the sensors (i.e. the :SensorNoise
   parameter is positive), so it may be warranted to hypothesize noisy
   words (words with some letters replaced in order to match the input
   stream). We only want to hypothesize these noisy words
   in `(get-more-hyps)` since to do so in `(hypothesize)` would cause
   an unwanted (and unwarranted) proliferation of hypotheses.

   The difficulty in offering noisy word hypotheses is that some
   hypotheses have already been accepted, and we also want to generate
   composite hypotheses that are based on the offered noisy hypotheses
   and the already accepted, and not accepted, hypotheses found in the
   workspace of `ep-state`. "
    [ep-state]
    (let [{:keys [dictionary models left-off centroid
                  indexed-letters avg-word-length accepted]}
          (:problem-data ep-state)
          ws (:workspace ep-state)
          max-n (apply max (keys models))
          ;; need sensor-hyps to be a seq for make-learning-hyp function
          sensor-hyps (sort-by (comp :pos :data) (:forced ws))
          last-time (apply max 0 (map (comp :pos :data) sensor-hyps))
          word-hyps (filter #(= :word (:type %)) (get-hyps ws :static))
          word-seq-hyps (filter #(= :word-seq (:type %)) (get-hyps ws :static))
          existing-hyps (concat word-hyps word-seq-hyps)
          ;; unexplained positions
          unexp-pos (set (map (comp :pos :data)
                              (set/intersection (find-unexplained ws) (:forced ws))))
          ;; positions also from any nearby word-hyps
          word-hyp-positions (apply concat (mapcat (comp :pos-seqs :data)
                                                   (find-adjacent-hyps unexp-pos word-hyps)))
          positions (sort (set/union unexp-pos
                                     (set (if (= -1 left-off) word-hyp-positions
                                              (map #(- % left-off) word-hyp-positions)))))
          sub-indexed-letters (sort-by first (map (fn [i] (nth indexed-letters i))
                                                  positions))
          sensor-noise-hyps (take (:MaxNoisyWords params)
                                  (sort-by :apriori
                                           (if (< 0 (:SensorNoise params))
                                             (make-sensor-noise-hyps
                                              sub-indexed-letters left-off
                                              dictionary max-n sensor-hyps
                                              accepted models) [])))
          learning-hyps (take (:MaxLearnedWords params)
                              (sort-by :apriori
                                       (if-not (and (> 100 (:BelievedKnowledge params))
                                                    (:Learn params))
                                         []
                                         (make-learning-hyps
                                          indexed-letters positions left-off
                                          dictionary sensor-hyps
                                          avg-word-length centroid last-time
                                          (concat word-hyps sensor-noise-hyps)))))
          composite-hyps (make-composite-hyps models sensor-noise-hyps accepted max-n)]
      ;; don't add any already-existing hyps
      (reduce (fn [ep hyp] (add-hyp ep hyp))
              ep-state (filter (fn [h] (not-any? #(hyps-equal? % h) existing-hyps))
                               (concat sensor-noise-hyps learning-hyps composite-hyps))))))

(comment
  (defn no-explainer-hyps
    [hyps pdata]
    []))

(comment
  (defn update-model
    [n model history words]
    (let [ws (concat (take-last (dec n) history) words)
          ngrams (map vec (partition n 1 ws))
          new-model (reduce (fn [m ngram] (if (get m ngram)
                                            (update-in m [ngram] inc)
                                            (assoc m ngram 1)))
                            model ngrams)]
      (with-meta new-model {:sum (reduce + (vals new-model))}))))

(comment
  (defn commit-decision
    [pdata accepted _ _ _]
    (let [words-pos-seqs
          (sort-by (comp first second)
                   (partition 2 (mapcat (fn [hyp] (interleave (:words (:data hyp))
                                                              (:pos-seqs (:data hyp))))
                                        accepted))) 
          [words left-off]
          (loop [words []
                 wps words-pos-seqs
                 end-time (:left-off pdata)]
            (let [[word pos-seq] (first wps)]
              (cond (empty? wps) [words end-time]
                    (>= end-time (first pos-seq))
                    (recur words (rest wps) end-time)
                    :else (recur (conj words word)
                                 (rest wps) (last pos-seq)))))
          learned-hyps (filter (fn [hyp] (= :learned-word (:subtype hyp))) accepted)
          learned-words (map (comp first :words :data) learned-hyps)
          models (:models pdata)
          history (:history pdata)
          max-n (:MaxModelGrams params)
          new-models (reduce (fn [ms i]
                               (assoc ms i
                                      (update-model i (get models i)
                                                    (concat (repeat (dec max-n) "") history)
                                                    words)))
                             {} (range 1 (inc max-n)))
          new-dict (set/union (:dictionary pdata) (set learned-words))
          new-features (update-features (:features pdata) new-dict)
          new-centroid (calc-centroid new-features (get new-models 1))
          new-accepted (set (concat (:accepted pdata)
                                    (filter #(not= :sensor (:type %)) accepted)))]
      (-> pdata
          (update-in [:history] concat words)
          (assoc :accepted new-accepted)
          (assoc :models new-models)
          (assoc :dictionary new-dict)
          (assoc :avg-word-length (if (empty? new-dict) 0
                                      (double (/ (reduce + (map count new-dict))
                                                 (count new-dict)))))
          (assoc :features new-features)
          (assoc :centroid new-centroid)
          (assoc :letters [])
          (assoc :left-off left-off)))))

(comment
  (defn retract
    [pdata hyp]
    pdata))
