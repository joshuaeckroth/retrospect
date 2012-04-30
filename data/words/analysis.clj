
(require 'retrospect.random)
(require 'retrospect.state)
(require 'retrospect.problems.words.truedata)
(require 'retrospect.reason.abduction.problems.words.evaluate)
(use 'loom.graph 'loom.alg 'loom.io 'clojure.set)
(use '[clojure.string :only (split)])

(defn build-markov-models
  "Build a Markov n-gram model of character transitions."
  [sentences max-ngram]
  (let [chars (map str (concat [" "] (apply concat (interpose " " (apply concat sentences))) [" "]))
        chars-grouped (filter (fn [cs] (some #(not= " " %) cs))
                              (apply concat (for [i (range 2 (inc max-ngram))] (partition i 1 (drop i chars)))))]
    (reduce (fn [ms cs] (let [prior (get-in ms [(count cs) cs] 0)]
                          (assoc-in ms [(count cs) cs] (inc prior))))
            (reduce (fn [ms i] (assoc ms i {})) {} (range 2 (inc max-ngram)))
            chars-grouped)))

(defn lattice
  [words]
  (reduce (fn [g word] (reduce (fn [g2 e]
                                 (let [w (or (apply weight g e) 0)]
                                   (add-edges g2 (conj e (inc w)))))
                               g (conj (map vec (partition 2 1 word))
                                       ["start" (first word)]
                                       [(last word) "end"])))
          (weighted-digraph)
          words))

(defn word-trans
  [sentences]
  (frequencies (mapcat (fn [sent] (map (fn [[w1 w2]] [(last w1) (first w2)])
                                       (partition 2 1 sent)))
                       sentences)))

(defn word-trans-test
  [sentences wtc-set]
  (map (fn [sent]
         (split
          (let [pairs (partition 2 1 (apply str sent))]
            (if (empty? pairs) (str (first sent))
                (apply str (concat [(ffirst pairs)]
                                   (mapcat (fn [[sym1 sym2]]
                                             (if (wtc-set [sym1 sym2])
                                               [\space sym2] [sym2]))
                                           pairs)))))
          #" "))
       sentences))

(defn char-trans-test
  [sentences char-trans]
  (map (fn [sent]
         (split
          (let [pairs (partition 2 1 (apply str sent))]
            (if (empty? pairs) (str (first sent))
                (apply str (concat [(ffirst pairs)]
                                   (mapcat (fn [[sym1 sym2]]
                                             (if (char-trans [sym1 sym2])
                                               [sym2] [\space sym2]))
                                           pairs)))))
          #" "))
       sentences))

(defn char-stats-test
  [sentences in-word wtc prefixes-prob suffixes-prob threshold]
  (map (fn [sent]
         (let [parse (split
                      (let [pairs (partition 2 1 (apply str sent))]
                        (if (empty? pairs) (str (first sent))
                            (apply str (concat [(ffirst pairs)]
                                               (mapcat (fn [[sym1 sym2]]
                                                         (cond (in-word [sym1 sym2]) [sym2]
                                                               (get wtc [sym1 sym2]) [\space sym2]
                                                               :else [sym2]))
                                                       pairs)))))
                      #" ")]
           (loop [ws parse
                  sent []]
             (cond (empty? ws) sent
                   (nil? (second ws)) (conj sent (first ws))
                   :else
                   (let [w1 (first ws)
                         w2 (second ws)]
                     (if (and (> (get prefixes-prob w1 0.0) threshold)
                              (> (get suffixes-prob w2 0.0) threshold))
                       (recur (rest (rest ws)) (conj sent (str w1 w2)))
                       (recur (rest ws) (conj sent w1))))))))
       sentences))

(defn std-dev [samples]
  (let [n (count samples)
	mean (/ (reduce + samples) n)
	intermediate (map #(Math/pow (- %1 mean) 2) samples)]
    (Math/sqrt 
     (/ (reduce + intermediate) n))))

(comment ["carroll" "pku_training" "as_training" "cityu_training" "msr_training"])

(defn char-clustering-test
  []
  (doseq [dataset ["pku_training"]]
    (doseq [i (range 1)]
      (println (format "%s %d/5... " dataset (inc i)))
      (binding [retrospect.random/rgen (retrospect.random/new-seed (rand-int 10000000))
                retrospect.state/params {:Dataset dataset :MaxModelGrams 0 :StatsOnly true
                                         :simulation (rand-int 100000)}
                retrospect.state/datadir (ref "data")]
        (let [truedata (retrospect.problems.words.truedata/generate-truedata)]
          (spit "chars.txt" (clojure.string/join "\n" (map #(apply str (interpose " " (seq (apply str %))))
                                                           (:sentences (:training truedata))))))))))

(defn clustering-test
  []
  (doseq [dataset ["pku_training"]]
    (doseq [i (range 1)]
      (println (format "%s %d/5... " dataset (inc i)))
      (binding [retrospect.random/rgen (retrospect.random/new-seed (rand-int 10000000))
                retrospect.state/params {:Dataset dataset :MaxModelGrams 0 :StatsOnly true
                                         :simulation (rand-int 100000)}
                retrospect.state/datadir (ref "data")]
        (let [truedata (retrospect.problems.words.truedata/generate-truedata)
              word-composites (map (fn [word] (retrospect.problems.words.truedata/find-inner-words
                                               word (:dictionary-no-composites (:training truedata))))
                                   (filter #(not= " " %) (apply concat (:sentences (:training truedata)))))]
          (spit "test.txt" (clojure.string/join "\n" (map (fn [comps] (clojure.string/join "\n" (map #(clojure.string/join " " %) comps))) word-composites))))))))

(defn markov-test
  []
  (doseq [dataset ["pku_training"]]
    (doseq [i (range 5)]
      (println (format "%s %d/5... " dataset (inc i)))
      (with-open [r (java.io.FileWriter. (format "data/words/char-analysis-tests-%s.csv" dataset)
                                         (if (= 0 i) false true))]
        (binding [retrospect.random/rgen (retrospect.random/new-seed (rand-int 10000000))
                  retrospect.state/params {:Dataset dataset :MaxModelGrams 0 :StatsOnly true
                                           :simulation (rand-int 100000)}
                  retrospect.state/datadir (ref "data")]
          (let [truedata (retrospect.problems.words.truedata/generate-truedata)
                max-ngram 3
                markov-models (build-markov-models (:sentences (:training truedata)) max-ngram)
                parse-fn (fn [ambiguous]
                           (loop [i 0
                                  n max-ngram
                                  marks []]
                             (let [char-subset (take i ambiguous)
                                   this-char (last char-subset)]
                               (cond (>= i (count ambiguous)) marks
                                     (= n 1) (recur (inc i) max-ngram (conj marks [:nospace 0]))
                                     :else
                                     (let [freq-nospace (get-in markov-models
                                                                [n (map str (take-last n char-subset))] 0)
                                           freq-space-before (get-in markov-models
                                                                     [n (map str (concat (butlast (take-last (dec n) char-subset))
                                                                                         [" "] [this-char]))] 0)
                                           freq-space-after (get-in markov-models
                                                                    [n (map str (concat (take-last (dec n) char-subset) [" "]))] 0)]
                                       (cond (and (> freq-space-before freq-space-after)
                                                  (> freq-space-before freq-nospace))
                                             (recur (inc i) max-ngram (conj marks [:space-before freq-space-before]))
                                             (and (> freq-space-after freq-space-before)
                                                  (> freq-space-after freq-nospace))
                                             (recur (inc i) max-ngram (conj marks [:space-after freq-space-after]))
                                             (and (> freq-nospace freq-space-after)
                                                  (> freq-nospace freq-space-before))
                                             (recur (inc i) max-ngram (conj marks [:nospace freq-nospace]))
                                             :else
                                             (recur i (dec n) marks)))))))
                parsed-sents (map (fn [ambiguous] (let [left-right (parse-fn ambiguous)
                                                        right-left (reverse (parse-fn (reverse ambiguous)))]
                                                    (map (fn [i] [(nth ambiguous i) [(nth left-right i) (nth right-left i)]])
                                                         (range (count ambiguous)))))
                                  (map #(get (:test truedata) %) (sort (keys (:test truedata)))))
                sents (map (fn [parse] (let [s (apply str (map (fn [[c [mark1 mark2]]]
                                                                 (let [[choice1 freq1] mark1
                                                                       [choice2 freq2] mark2
                                                                       choice (if (> freq1 freq2)
                                                                                choice1
                                                                                ;; if choosing result from reversed text,
                                                                                ;; flip the result
                                                                                (cond (= :space-after choice2) :space-before
                                                                                      (= :space-before choice2) :space-after
                                                                                      :else :nospace))]
                                                                   (cond (= choice :nospace) c
                                                                         (= choice :space-before) (str \space c)
                                                                         (= choice :space-after) (str c \space))))
                                                               parse))]
                                         (split s #"\s+")))
                           parsed-sents)]
            (println (retrospect.reason.abduction.problems.words.evaluate/run-scorer
                      (:test-sentences truedata) sents (:dictionary (:training truedata))))))))))

(defn char-stats
  []
  (doseq [dataset ["pku_training"]]
    (doseq [i (range 5)]
      (println (format "%s %d/5... " dataset (inc i)))
      (with-open [r (java.io.FileWriter. (format "data/words/char-analysis-tests-%s.csv" dataset)
                                         (if (= 0 i) false true))]
        (binding [retrospect.random/rgen (retrospect.random/new-seed (rand-int 10000000))
                  retrospect.state/params {:Dataset dataset :MaxModelGrams 0 :StatsOnly true
                                           :simulation (rand-int 100000)}
                  retrospect.state/datadir (ref "data")]
          (let [truedata (retrospect.problems.words.truedata/generate-truedata)
                oov (set (filter #(not ((:dictionary (:training truedata)) %))
                                 (apply concat (:test-sentences truedata))))

                word-freqs (frequencies (apply concat (:sentences (:training truedata))))
                dict-composites (mapcat #(retrospect.problems.words.truedata/find-inner-words % (:dictionary (:training truedata)))
                                        (:dictionary (:training truedata)))
                prefixes (map first (filter second dict-composites))
                suffixes (map last (filter second dict-composites))
                middles (mapcat #(butlast (rest %)) (filter #(< 2 (count %)) dict-composites))
                prefix-suffix-freqs (frequencies (apply concat dict-composites))

                prefixes-freq (frequencies prefixes)
                suffixes-freq (frequencies suffixes)
                middles-freq (frequencies middles)

                prefixes-prob (reduce (fn [m w] (assoc m w (/ (double (get prefixes-freq w))
                                                              (double (+ (get prefix-suffix-freqs w)
                                                                         (get word-freqs w 0))))))
                                      {} (keys prefixes-freq))
                suffixes-prob (reduce (fn [m w] (assoc m w (/ (double (get suffixes-freq w))
                                                              (double (+ (get prefix-suffix-freqs w)
                                                                         (get word-freqs w 0))))))
                                      {} (keys suffixes-freq))

                ;; in-word transitions
                dtg (:dtg (:training truedata))
                in-word (set (filter #(and (not= "start" (first %))
                                           (not= "end" (second %)))
                                     (edges dtg)))
                start-end-chars (set (filter #(or (= "start" (first %))
                                                  (= "end" (second %)))
                                             (edges dtg)))
                dtg-oov (lattice oov)

                ;; word transitions
                wtc (:wtc (:training truedata))

                char-pairs (set (concat (filter #(and (not= "start" (first %))
                                                      (not= "end" (second %)))
                                                (edges dtg))
                                        (keys wtc)))
                oov-char-pairs (set (concat (filter #(and (not= "start" (first %))
                                                          (not= "end" (second %)))
                                                    (edges dtg-oov))
                                            (keys wtc)))

                char-pair-scores (reduce (fn [m cp]
                                           (assoc m cp
                                                  (/ (double (or (apply weight dtg cp) 0))
                                                     (double (+ (or (apply weight dtg cp) 0)
                                                                (get wtc cp 0))))))
                                         {} char-pairs)

                oov-inner-words (mapcat #(retrospect.problems.words.truedata/find-inner-words % (:dictionary (:training truedata))) oov)
                oov-prefixes (map first (filter second oov-inner-words))
                oov-suffixes (map last (filter second oov-inner-words))
                oov-middles (mapcat #(butlast (rest %)) (filter #(< 2 (count %)) oov-inner-words))

                oov-prefixes-freq (frequencies oov-prefixes)
                oov-suffixes-freq (frequencies oov-suffixes)
                oov-middles-freq (frequencies oov-middles)

                
                oov-word-freqs (frequencies (apply concat oov-inner-words))
                oov-composite-counts (map count oov-inner-words)
                oov-inner-word-wtc (frequencies (mapcat (fn [sent] (map (fn [[w1 w2]] [(last w1) (first w2)])
                                                                        (partition 2 1 sent)))
                                                        oov-inner-words))

                oov-inner-word-scores (filter identity (map (fn [cp] (get char-pair-scores cp))
                                                            (keys oov-inner-word-wtc)))
                oov-inner-new (count (filter nil? (map (fn [cp] (get char-pair-scores cp))
                                                       (keys oov-inner-word-wtc))))
                oov-inner-start (filter identity (map #(weight dtg "start" (second %))
                                                      (keys oov-inner-word-wtc)))
                oov-inner-end (filter identity (map #(weight dtg (first %) "end")
                                                    (keys oov-inner-word-wtc)))
                all-start (filter identity (map #(weight dtg "start" (second %))
                                                start-end-chars))
                all-end (filter identity (map #(weight dtg (first %) "end")
                                              start-end-chars))
                wtc-set (set (keys wtc))
                char-trans (set (edges dtg))
                wtc-set-only (difference wtc-set char-trans)]
            (comment wtc-test (word-trans-test (:test-sentences truedata) wtc-set-only)
                     char-test (char-trans-test (:test-sentences truedata) char-trans)
                     oov-got-from-wtc-test (intersection oov (set (apply concat wtc-test)))
                     oov-got-from-char-test (intersection oov (set (apply concat char-test)))
                     new-dict-wtc (difference (set (apply concat wtc-test)) (:dictionary (:training truedata)))
                     new-dict-char (difference (set (apply concat char-test)) (:dictionary (:training truedata))))
            (when (= i 0)
              (.write r "oov,prefixProb,suffixProb\n")
              (comment (.write r "oov,wordTrans,Wweight,WSweight,WEweight,CTweight\n")))
            (doseq [w (apply concat (:test-sentences truedata))]
              (let [composites (retrospect.problems.words.truedata/find-inner-words w (:dictionary (:training truedata)))]
                (doseq [cs (filter second composites)]
                  (.write r (format "%s,%f,%f\n"
                                    (if (oov w) "T" "F")
                                    (get prefixes-prob (first cs) 0.0)
                                    (get suffixes-prob (last cs) 0.0)))))
              (comment (let [cps (partition 2 1 w)]
                         (doseq [i (range (count cps))]
                           (let [cp (nth cps i)]
                             (.write r (format "%s,%s,%d,%d,%d,%d\n"
                                               (if (oov w) "T" "F")
                                               (if (or (= i 0) (= i (dec (count cps)))) "T" "F")
                                               (get wtc cp 0)
                                               (or (weight dtg "start" (second cp)) 0)
                                               (or (weight dtg (first cp) "end") 0)
                                               (or (weight dtg (first cp) (second cp)) 0))))))))
            (comment
              (println "wtc"
                       (/ (double (count (intersection oov-got-from-wtc-test oov-got-from-char-test)))
                          (double (count oov-got-from-wtc-test)))
                       (/ (double (count oov-got-from-wtc-test))
                          (double (count oov)))
                       (/ (double (count (intersection new-dict-wtc new-dict-char)))
                          (double (count new-dict-wtc)))
                       (/ (double (count (intersection new-dict-wtc oov)))
                          (double (count new-dict-wtc)))
                       "char"
                       (/ (double (count (intersection oov-got-from-wtc-test oov-got-from-char-test)))
                          (double (count oov-got-from-char-test)))
                       (/ (double (count oov-got-from-char-test))
                          (double (count oov)))
                       (/ (double (count (intersection new-dict-wtc new-dict-char)))
                          (double (count new-dict-char)))
                       (/ (double (count (intersection new-dict-char oov)))
                          (double (count new-dict-char)))))
            (comment
              (when (= i 0)
                (.write r "oov,type,freq\n"))
              (doseq [[_ f] (seq prefixes-freq)]
                (.write r (format "F,pre,%d\n" f)))
              (doseq [[_ f] (seq suffixes-freq)]
                (.write r (format "F,suf,%d\n" f)))
              (doseq [[_ f] (seq middles-freq)]
                (.write r (format "F,mid,%d\n" f)))
              (doseq [[_ f] (seq oov-prefixes-freq)]
                (.write r (format "T,pre,%d\n" f)))
              (doseq [[_ f] (seq oov-suffixes-freq)]
                (.write r (format "T,suf,%d\n" f)))
              (doseq [[_ f] (seq oov-middles-freq)]
                (.write r (format "T,mid,%d\n" f))))
            (comment
              (println "count oov composites" (/ (double (reduce + oov-composite-counts))
                                                 (double (count oov-composite-counts))))
              (println "occur oov start" (/ (double (reduce + oov-inner-start))
                                            (double (count oov-inner-start))))
              (println "occur all start" (/ (double (reduce + all-start))
                                            (double (count all-start))))
              (println "occur oov end" (/ (double (reduce + oov-inner-end))
                                          (double (count oov-inner-end))))
              (println "occur all end" (/ (double (reduce + all-end))
                                          (double (count all-end))))
              (println "new oov word-trans" (/ (double oov-inner-new)
                                               (double (count (keys oov-inner-word-wtc))))))
            (comment
              (when (= i 0)
                (.write r "freq\n"))
              (doseq [w (keys oov-word-freqs)]
                (.write r (format "%d\n" (get oov-word-freqs w)))))
            (comment (when (= i 0)
                       (.write r "oov,se,occur\n"))
                     (doseq [s oov-inner-start]
                       (.write r (format "T,s,%d\n" s)))
                     (doseq [s oov-inner-end]
                       (.write r (format "T,e,%d\n" s)))
                     (doseq [s all-start]
                       (.write r (format "F,s,%d\n" s)))
                     (doseq [s all-end]
                       (.write r (format "F,e,%d\n" s))))
            (comment (when (= i 0)
                       (.write r "threshold,prec,recall,fscore,oovrate,oovrecall,ivrecall\n"))
                     (doseq [threshold (range 0.0 1.01 0.1)]
                       (let [[prec recall f-score oov-rate oov-recall iv-recall]
                             (retrospect.reason.abduction.problems.words.evaluate/run-scorer
                              (:test-sentences truedata)
                              (char-stats-test (:test-sentences truedata) in-word wtc
                                               prefixes-prob suffixes-prob threshold)
                              (:dictionary (:training truedata)))]
                         (.write r (format "%f,%f,%f,%f,%f,%f,%f\n"
                                           threshold prec recall f-score
                                           oov-rate oov-recall iv-recall)))))))))))

(defn stats
  []
  (doseq [dataset ["carroll" "pku_training" "as_training" "cityu_training" "msr_training"]]
    (doseq [i (range 5)]
      (println (format "%s %d/5... " dataset (inc i)))
      (with-open [r (java.io.FileWriter. (format "data/words/words-analysis-%s.csv" dataset)
                                         (if (= 0 i) false true))]
        (binding [retrospect.random/rgen (retrospect.random/new-seed (rand-int 10000000))
                  retrospect.state/params {:Dataset dataset :MaxModelGrams 0 :StatsOnly true
                                           :simulation (rand-int 100000)}
                  retrospect.state/datadir (ref "data")]
          (let [truedata (retrospect.problems.words.truedata/generate-truedata)
                oov (set (filter #(not ((:dictionary (:training truedata)) %))
                                 (apply concat (:test-sentences truedata))))
                oov-composite [] #_(filter #(composite-of-words? % (:dictionary (:training truedata))) oov)
                dtg (:dtg (:training truedata))
                dtg-oov (lattice oov)

                chars (set (apply concat (:dictionary (:training truedata))))
                chars-oov (set (apply concat oov))

                chars-only-start (filter (fn [c] (every? (fn [[e1 _]] (= "start" e1))
                                                         (filter (fn [[_ e2]] (= c e2)) (edges dtg))))
                                         chars)
                chars-only-end (filter (fn [c] (every? (fn [[_ e2]] (= "end" e2))
                                                       (filter (fn [[e1 _]] (= c e1)) (edges dtg))))
                                       chars)
                
                ;; oov words not COMPLETELY represented by graph transitions
                oov-some-notrans (filter (fn [word] (some (fn [e] (not (apply has-edge? dtg e)))
                                                          (conj (map vec (partition 2 1 word))
                                                                ["start" (first word)]
                                                                [(last word) "end"])))
                                         oov)
                ;; oov words completely NOT represented by graph transitions
                oov-all-notrans (filter (fn [word] (every? (fn [e] (not (apply has-edge? dtg e)))
                                                           (conj (map vec (partition 2 1 word))
                                                                 ["start" (first word)]
                                                                 [(last word) "end"])))
                                        oov)
                ;; word transitions at the character level
                wtc (:wtc (:training truedata))
                wtc-set (set (keys wtc))
                ;; word transitions in the character lattice
                wtc-in-lattice (filter (fn [[e1 e2]] (has-edge? dtg e1 e2)) (keys wtc))
                wtc-in-oov-lattice (filter (fn [[e1 e2]] (has-edge? dtg-oov e1 e2)) (keys wtc))

                wtc-in-lattice-freq-avg (/ (double (reduce + (map #(get wtc %) wtc-in-lattice)))
                                           (double (count wtc-in-lattice)))
                wtc-in-lattice-freq-sd (std-dev (map #(get wtc %) wtc-in-lattice))
                wtc-in-lattice-freq-median (nth (sort (map #(get wtc %) wtc-in-lattice))
                                                (int (/ (count wtc-in-lattice) 2)))

                wtc-test (word-trans (:test-sentences truedata))
                wtc-test-in-lattice (filter (fn [[e1 e2]] (has-edge? dtg e1 e2)) (keys wtc-test))
                wtc-test-in-oov-lattice (filter (fn [[e1 e2]] (has-edge? dtg-oov e1 e2)) (keys wtc-test))

                char-trans (set (edges dtg))
                char-trans-no-start-end (set (filter #(and (not= "start" (first %))
                                                           (not= "end" (second %)))
                                                     (edges dtg)))
                char-trans-only (difference char-trans-no-start-end wtc-set)
                wtc-set-only (difference wtc-set char-trans)                
                oov-char-trans (set (edges dtg-oov))
                wtc-oov-char-trans (intersection wtc-set oov-char-trans)
                wtc-oov-char-trans-freq-avg (/ (double (reduce + (map #(get wtc %) wtc-oov-char-trans)))
                                               (double (count wtc-oov-char-trans)))
                wtc-oov-char-trans-freq-sd (std-dev (map #(get wtc %) wtc-oov-char-trans))
                wtc-oov-char-trans-freq-median (nth (sort (map #(get wtc %) wtc-oov-char-trans))
                                                    (int (/ (count wtc-oov-char-trans) 2)))
                oov-char-trans-no-start-end (set (filter #(and (not= "start" (first %))
                                                               (not= "end" (second %)))
                                                         (edges dtg-oov)))
                [wtc-prec wtc-recall wtc-f-score
                 wtc-oov-rate wtc-oov-recall wtc-iv-recall]
                (retrospect.reason.abduction.problems.words.evaluate/run-scorer
                 (:test-sentences truedata)
                 (word-trans-test (:test-sentences truedata) wtc-set)
                 (:dictionary (:training truedata)))

                [wtc-only-prec wtc-only-recall wtc-only-f-score
                 wtc-only-oov-rate wtc-only-oov-recall wtc-only-iv-recall]
                (retrospect.reason.abduction.problems.words.evaluate/run-scorer
                 (:test-sentences truedata)
                 (word-trans-test (:test-sentences truedata) wtc-set-only)
                 (:dictionary (:training truedata)))

                [char-prec char-recall char-f-score
                 char-oov-rate char-oov-recall char-iv-recall]
                (retrospect.reason.abduction.problems.words.evaluate/run-scorer
                 (:test-sentences truedata)
                 (char-trans-test (:test-sentences truedata) char-trans)
                 (:dictionary (:training truedata)))

                [char-only-prec char-only-recall char-only-f-score
                 char-only-oov-rate char-only-oov-recall char-only-iv-recall]
                (retrospect.reason.abduction.problems.words.evaluate/run-scorer
                 (:test-sentences truedata)
                 (char-trans-test (:test-sentences truedata) char-trans-only)
                 (:dictionary (:training truedata)))]
            (when (= i 0)
              (.write r
                      (str "words,sentences,wordsPerSent,charsPerWord,chars,"
                           "charTrans,charTransNoStartEnd,charsOnlyStart,charsOnlyEnd,"
                           "oovChars,oovCharTrans,oovCharTransNoStartEnd,oovComposite,oovSomeNotrans,oovAllNotrans,"

                           "wordTransInLattice,wordTransInLatticeFreqAvg,wordTransInLatticeFreqMedian,wordTransInLatticeFreqSD,"
                           
                           "oovSharedCharTrans,oovSharedCharTransNoStartEnd,"
                           "wordTransInLatticePct,wordTransInOOVLattice,OOVCharTransAreWordTrans,"
                           "OOVCharTransWordTrans,OOVCharTransWordTransFreqAvg,OOVCharTransWordTransFreqMedian,OOVCharTransWordTransFreqSD,"
                           "wordTransTestInLattice,wordTransTestInOOVLattice,wordTransTestInWordTrans,"
                           
                           "wordTransPrec,wordTransRecall,wordTransFScore,wordTransOOVRate,"
                           "wordTransOOVRecall,wordTransIVRecall,"
                           "wordTransOnlyPrec,wordTransOnlyRecall,wordTransOnlyFScore,wordTransOnlyOOVRate,"
                           "wordTransOnlyOOVRecall,wordTransOnlyIVRecall,"
                           "charTransPrec,charTransRecall,charTransFScore,charTransOOVRate,"
                           "charTransOOVRecall,charTransIVRecall,"
                           "charTransOnlyPrec,charTransOnlyRecall,charTransOnlyFScore,charTransOnlyOOVRate,"
                           "charTransOnlyOOVRecall,charTransOnlyIVRecall"
                           "\n")))
            (.write r
                    (format (str "%d,%d,%f,%f,%d,"
                                 "%d,%d,%f,%f,"
                                 "%d,%d,%d,%f,%f,%f,"

                                 "%d,%f,%d,%f,"
                                 
                                 "%f,%f,"
                                 "%f,%f,%f,"
                                 "%d,%f,%d,%f,"
                                 "%f,%f,%f,"
                                 
                                 "%f,%f,%f,%f,%f,%f,"
                                 "%f,%f,%f,%f,%f,%f,"
                                 "%f,%f,%f,%f,%f,%f,"
                                 "%f,%f,%f,%f,%f,%f"
                                 "\n")
                            (count (:dictionary (:training truedata)))
                            (count (:sentences (:training truedata)))
                            (/ (double (reduce + (map count (:sentences (:training truedata)))))
                               (double (count (:sentences (:training truedata)))))
                            (/ (double (reduce + (map count (:dictionary (:training truedata)))))
                               (double (count (:dictionary (:training truedata)))))
                            (count chars)
                            (count (edges dtg))
                            (count char-trans-no-start-end)
                            (/ (double (count chars-only-start)) (double (count chars)))
                            (/ (double (count chars-only-end)) (double (count chars)))

                            (count chars-oov)
                            (count (edges dtg-oov))
                            (count oov-char-trans-no-start-end)
                            (/ (double (count oov-composite)) (double (count oov)))
                            (/ (double (count oov-some-notrans)) (double (count oov)))
                            (/ (double (count oov-all-notrans)) (double (count oov)))

                            (count wtc-in-lattice)
                            wtc-in-lattice-freq-avg
                            wtc-in-lattice-freq-median
                            wtc-in-lattice-freq-sd

                            (/ (double (count (intersection char-trans oov-char-trans)))
                               (double (count oov-char-trans)))
                            (/ (double (count (intersection char-trans-no-start-end oov-char-trans-no-start-end)))
                               (double (count oov-char-trans-no-start-end)))
                            
                            (/ (double (count wtc-in-lattice)) (double (count wtc)))
                            (/ (double (count wtc-in-oov-lattice)) (double (count wtc)))
                            (/ (double (count wtc-oov-char-trans)) (double (count oov-char-trans)))

                            (count wtc-oov-char-trans)
                            wtc-oov-char-trans-freq-avg
                            wtc-oov-char-trans-freq-median
                            wtc-oov-char-trans-freq-sd

                            (/ (double (count wtc-test-in-lattice)) (double (count wtc-test)))
                            (/ (double (count wtc-test-in-oov-lattice)) (double (count wtc-test)))
                            (/ (double (count (intersection (set (keys wtc-test)) (set (keys wtc)))))
                               (double (count wtc-test)))

                            wtc-prec wtc-recall wtc-f-score
                            wtc-oov-rate wtc-oov-recall wtc-iv-recall

                            wtc-only-prec wtc-only-recall wtc-only-f-score
                            wtc-only-oov-rate wtc-only-oov-recall wtc-only-iv-recall

                            char-prec char-recall char-f-score
                            char-oov-rate char-oov-recall char-iv-recall

                            char-only-prec char-only-recall char-only-f-score
                            char-only-oov-rate char-only-oov-recall char-only-iv-recall))))))))

