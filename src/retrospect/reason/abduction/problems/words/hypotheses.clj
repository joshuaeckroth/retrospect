(ns retrospect.reason.abduction.problems.words.hypotheses
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [retrospect.profile :only [prof]])
  (:use [clojure.contrib.combinatorics :only [combinations]])
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
            (let [words-grouped (apply concat (for [i (range 1 (inc (:MaxModelGrams params)))]
                                                (partition i (concat (repeat (dec i) "") sentence))))]
              (reduce (fn [ms ws] (let [m (get ms (count ws) {})
                                        prior (get m ws 0)]
                                    (assoc-in ms [(count ws) ws] (inc prior))))
                      models words-grouped)))
          {} training))

(defn build-symbol-tendencies
  [training]
  (letfn [(inc-sym [m sym pos] (if (nil? (get (pos m) sym))
                                 (assoc-in m [pos sym] 1)
                                 (update-in m [pos sym] inc)))]
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
                     kb {:avg-word-length (if (empty? training-dict) 0
                                              (double (/ (reduce + (map count training-dict))
                                                         (count training-dict))))
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

(def cache (ref nil))

(defn reset
  [] (dosync (alter cache (constantly nil))))

(defn update-cache
  [hyps]
  (prof :update-cache
        (let [kb (get-kb hyps)
              sensor-hyps (vec (sort-by :pos (get hyps :sensor)))
              sensor-str (apply str (map :symbol sensor-hyps))
              words (set (mapcat (fn [sym] (get (:symbol-words kb) sym)) (seq sensor-str)))
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
          (dosync (alter cache (constantly word-positions))))))

              (comment nearby-sensor-hyps (prof :nearby-sensor-hyps
                                                (reverse (sort-by count matches-near-evidence))))

(defn find-word
  [evidence other-hyps]
  (prof :find-word
        (let [matches-near-evidence (prof :matches-near-evidence
                                          (filter #(and (<= (:pos (first %)) (:pos evidence))
                                                        (>= (:pos (last %)) (:pos evidence)))
                                                  @cache))]
          (prof :find-word-filter
                (filter (fn [sensor-hyps]
                          (not-any? #(same-word-hyp (map :pos sensor-hyps)
                                                    (apply str (map :symbol sensor-hyps)) %)
                                    other-hyps))
                        matches-near-evidence)))))

(defn dnorm
  [x mean stddev]
  (* (/ 1.0 (* stddev (Math/sqrt (* 2 Math/PI))))
     (Math/exp (- (/ (Math/pow (- x mean) 2.0)
                     (* 2.0 stddev stddev))))))

(defn gaussian-mixture-true
  [x]
  (+ (* 0.6627 (dnorm x 0.4380 0.1446))
     (* 0.3445 (dnorm x -0.1025 0.3917))))

(defn gaussian-mixture-false
  [x]
  (+ (* 0.4615 (dnorm x -0.8127 0.1103))
     (* 0.6064 (dnorm x -0.3075 0.5866))))

(defn word-metrics
  [kb word]
  (let [avg-word-length (:avg-word-length kb)
        symbol-tendencies (:symbol-tendencies kb)
        tendency (prof :tendency
                       (/ (reduce
                           + (map (fn [i]
                                    (let [sym (nth word i)
                                          occur (double (get (:occur symbol-tendencies) sym 1.0))
                                          get-pos (fn [pos] (get (pos symbol-tendencies) sym))]
                                      (cond (= i 0)
                                            (/ (double (or (get-pos :front) 0.0)) occur)
                                            (= i (dec (count word)))
                                            (/ (double (or (get-pos :back) 0.0)) occur)
                                            :else
                                            (/ (double (or (get-pos :middle) 0.0)) occur))))
                                  (range (count word))))
                          (double (count word))))
        length-diff (Math/abs (double (- avg-word-length (count word))))
        length-diff-pct (/ length-diff avg-word-length)
        calc (/ (- tendency length-diff-pct) (+ tendency length-diff-pct))
        pdf-true (gaussian-mixture-true calc)
        pdf-false (gaussian-mixture-false calc)]
    [(/ pdf-true (+ pdf-true pdf-false))
     (format (str "\nLength difference: %.2f"
                  "\nLength difference %%: %.2f"
                  "\nTendency measure: %.2f"
                  "\nCalc: %.2f"
                  "\nPdf true: %.2f"
                  "\nPdf false: %.2f")
             length-diff length-diff-pct tendency calc pdf-true pdf-false)
     length-diff-pct tendency]))

(defmethod hypothesize :sensor
  [evidence accepted rejected hyps]
  (when (nil? @cache) (update-cache hyps))
  (prof :hyp-word
        (let [other-hyps (concat (get hyps :word) (get hyps :word-seq))]
          (let [expls (find-word evidence other-hyps)]
            (map (fn [expl]
                   (let [w (apply str (map :symbol expl))
                         kb (get-kb hyps)
                         [metric-prob metric-desc length-diff-pct tendency] (word-metrics kb w)
                         unigram-model (get (:models kb) 1)
                         substrings (:substrings kb)
                         similar-words (get substrings w)
                         similar-sum (reduce + (map (fn [w2] (get unigram-model [w2]))
                                                    similar-words))]
                     (new-hyp "Word" :word :word true conflicts
                              (* metric-prob (double (/ (get unigram-model [w]) similar-sum)))
                              expl [] w
                              (format "Word \"%s\" (pos %d-%d)\nmetric-prob: %.2f\n%s"
                                      w (:pos (first expl))
                                      (:pos (last expl))
                                      metric-prob
                                      metric-desc)
                              {:word w :pos-seq (map :pos expl)
                               :length-diff-pct length-diff-pct
                               :tendency tendency})))
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

(defn score-learned-word
  [kb word]
  (prof :score-learned-word
        (let [[prob desc length-diff-pct tendency] (word-metrics kb word)
              apriori (* (:LearnMultiplier params) prob)]
          [apriori desc length-diff-pct tendency])))

(defmulti learn
  (fn [evidence no-explainer-hyps hyps] (:type evidence)))

(defmethod learn :default [_ _ _] nil)

(defmethod learn :sensor
  [evidence unexp hyps]
  (prof :hyp-learn-word
        (let [sensor-unexp (filter #(= :sensor (:type %)) unexp)
              unexp-left (reverse (sort-by :pos (filter #(< (:pos %) (:pos evidence)) sensor-unexp)))
              unexp-right (sort-by :pos (filter #(> (:pos %) (:pos evidence)) sensor-unexp))
              left-pairs (take-while #(or (nil? (second %))
                                          (= (inc (:pos (first %))) (:pos (second %))))
                                     (partition-all 2 1 unexp-left))
              right-pairs (take-while #(or (nil? (second %))
                                           (= (inc (:pos (first %))) (:pos (second %))))
                                      (partition-all 2 1 unexp-right))
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
                  [apriori learn-desc length-diff-pct tendency]
                  (score-learned-word kb word)]
              [(new-hyp "LearnedWord" :word :learned-word true conflicts
                        apriori expl [] word
                        (format (str "Learned word: \"%s\" (pos %d-%d)"
                                     "\nTo explain: %s"
                                     "\n%s")
                                word (first pos-seq) (last pos-seq)
                                (str (:symbol evidence)) learn-desc)
                        {:word word :pos-seq pos-seq
                         :length-diff-pct length-diff-pct
                         :tendency tendency})])))))

(defn get-left-hyps
  [evidence hyps]
  (prof :get-left-hyps
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
            left-hyps))))

(defn get-right-hyps
  [evidence hyps]
  (prof :get-right-hyps
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
  [evidence unexp hyps]
  (prof :hyp-learn-word-seq
        (let [left (take-last 5 (or (get-left-hyps evidence unexp) []))
              right (take 5 (or (get-right-hyps evidence unexp) []))
              kb (get-kb hyps)
              expls (expl-seqs left evidence right)]
          (loop [es expls
                 hs []]
            (if (empty? es) (when (not-empty hs) hs)
                (let [expl (first es)
                      word (apply str (map :word expl))]
                  (cond (and (not-any? #(same-word-hyp (mapcat :pos-seq expl) word %)
                                       (concat hs (get hyps :word)))
                             (nil? (get (get (:models kb) 1) [word])))
                        (let [[apriori learn-desc length-diff-pct tendency]
                              (score-learned-word kb word)
                              hyp (new-hyp "LWWord" :word :learned-word true conflicts
                                           apriori (mapcat :explains expl) [] word
                                           (format (str "Learned word from existing words: \"%s\" (pos %d-%d)"
                                                        "\nTo explain: %s\nEntire sequence: %s"
                                                        "\n%s")
                                                   word (first (:pos-seq (first expl)))
                                                   (last (:pos-seq (last expl)))
                                                   evidence (str/join ", " expl) learn-desc)
                                           {:word word :pos-seq (mapcat :pos-seq expl)
                                            :length-diff-pct length-diff-pct
                                            :tendency tendency})]
                          (recur (rest es) (conj hs hyp)))
                        (and (nil? (get (get (:models kb) (count expl)) (map :word expl)))
                             (not-any? (fn [h] (hyps-equal? h {:type :word-seq
                                                               :pos-seqs (map :pos-seq expl)
                                                               :words (map :word expl)}))
                                       (concat hs (get hyps :word-seq))))
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
                        :else (recur (rest es) hs))))))))
