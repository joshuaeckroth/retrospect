(ns retrospect.reason.abduction.problems.words.evaluate
  (:use [clojure.java.shell :only [sh]])
  (:use [clojure.string :only [join]])
  (:use [retrospect.reason.abduction.evaluate :only [calc-increase]])
  (:use [retrospect.problems.words.symbols])
  (:use [retrospect.epistemicstates :only [cur-ep flatten-est]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata time-now hyp]
  (if-not (or (= :word (:type hyp)) (= :word-seq (:type hyp))) true
          (let [sentence (nth (:test-sentences truedata) (dec time-now))
                start-pos (if (= :word (:type hyp)) (first (:pos hyp))
                              (ffirst (:pos-seqs hyp)))
                end-pos (if (= :word (:type hyp)) (last (:pos hyp))
                            (last (last (:pos-seqs hyp))))
                hyp-words (if (= :word (:type hyp)) [(:word hyp)] (:words hyp))
                true-words (loop [i 0 ws [] sent sentence]
                             (cond (empty? sent) ws
                                   (> i end-pos) ws
                                   (< i start-pos) (recur (+ i (count (first sent))) ws (rest sent))
                                   :else (recur (+ i (count (first sent))) (conj ws (first sent))
                                                (rest sent))))]
            (= hyp-words true-words))))

(defn hyps-equal?
  [hyp1 hyp2]
  (if (not= (:type hyp1) (:type hyp2)) false
      (apply = (map #(select-keys % [:words :pos]) [hyp1 hyp2]))))

(defn get-history
  [accepted]
  (map (fn [h] (if (= :word (:type h)) (:word h) (str (:symbol h))))
       (sort-by (comp first :pos)
                (concat (get accepted :word)
                        (get accepted :punctuation)))))

(defn evaluate
  [truedata ors]
  (let [eps (rest (flatten-est (:est ors)))
        time-now (:time (last eps))
        believed (map (fn [ep] (filter #(not (re-matches punctuation-regex %))
                                       (get-history (:accepted (:workspace ep)))))
                      eps)
        sentences (map (fn [i] (filter #(not (re-matches punctuation-regex %))
                                       (nth (:test-sentences truedata) i)))
                       (range time-now))
        [prec recall f-score oov-rate oov-recall iv-recall]
        (try (do
               (spit "/tmp/truth.txt" (join "\n" (map #(join " " %) sentences))
                     :encoding "utf-8")
               (spit "/tmp/believed.txt" (join "\n" (map #(join " " %) believed))
                     :encoding "utf-8")
               (spit "/tmp/dictionary.txt" (join "\n" (sort (second (:training truedata))))
                     :encoding "utf-8")
               (let [results (sh (format "%s/words/score" @datadir)
                                 "/tmp/dictionary.txt" "/tmp/truth.txt" "/tmp/believed.txt")
                     prec (Double/parseDouble
                           (second (re-find #"=== TOTAL TEST WORDS PRECISION:\s+(\d\.\d\d\d)"
                                            (:out results))))
                     recall (Double/parseDouble
                             (second (re-find #"=== TOTAL TRUE WORDS RECALL:\s+(\d\.\d\d\d)"
                                              (:out results))))
                     f-score (Double/parseDouble
                              (second (re-find #"=== F MEASURE:\s+(\d\.\d\d\d)"
                                               (:out results))))
                     oov-rate (try (Double/parseDouble
                                    (second (re-find #"=== OOV Rate:\s+(\d\.\d\d\d)"
                                                     (:out results))))
                                   (catch Exception _ 0.0))
                     oov-recall (try (Double/parseDouble
                                      (second (re-find #"=== OOV Recall Rate:\s+(\d\.\d\d\d)"
                                                       (:out results))))
                                     (catch Exception _ 0.0))
                     iv-recall (try (Double/parseDouble
                                     (second (re-find #"=== IV Recall Rate:\s+(\d\.\d\d\d)"
                                                      (:out results))))
                                    (catch Exception _ 0.0))]
                 [prec recall f-score oov-rate oov-recall iv-recall]))
             (catch Exception e (do (log e) [-1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0])))]
    {:Prec prec
     :Recall recall
     :FScore f-score
     :OOVRate oov-rate
     :OOVRecall oov-recall
     :IVRecall iv-recall}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:LearnedCount :LearnedCorrect
                     :Prec :Recall :FScore :OOVRecall])))
