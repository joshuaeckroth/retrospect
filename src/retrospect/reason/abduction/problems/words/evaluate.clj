(ns retrospect.reason.abduction.problems.words.evaluate
  (:use [clojure.java.shell :only [sh]])
  (:use [retrospect.reason.abduction.evaluate :only [calc-increase]])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata pdata time hyp]
  (let [truewords-starts (filter #(<= (second %) time)
                                 (:word-starts (meta truedata)))
        words (:words (:data hyp))
        pos-seqs (:pos-seqs (:data hyp))]
    (every? (fn [i]
              (let [word (nth words i)
                    word-start (first (nth pos-seqs i))
                    tw (ffirst (filter #(= word-start (second %)) truewords-starts))]
                (= tw word)))
            (range (count words)))))

(defn hyps-equal?
  [hyp1 hyp2]
  (if (not= (:type hyp1) (:type hyp2)) false
      (apply = (map #(select-keys % [:words :pos-seqs]) [hyp1 hyp2]))))

(defn get-history
  [accepted]
  (mapcat :words (sort-by (comp ffirst :pos-seqs) (get accepted :word))))

(defn evaluate
  [accepted rejected time-now sensors truedata]
  (let [believed (get-history accepted)
        dict (:dictionary (meta truedata))
        learned (filter #(= :learned-word (:subtype %)) (get accepted :word))
        truewords-starts (filter #(<= (+ (second %) (count (first %)))
                                      time-now)
                                 (:word-starts (meta truedata)))
        truewords (map first truewords-starts)
        [prec recall f-score oov-recall]
        (try (do
               (spit "/tmp/truewords.txt" (apply str (interpose " " truewords))
                     :encoding (:Encoding params))
               (spit "/tmp/history.txt" (apply str (interpose " " believed))
                     :encoding (:Encoding params))
               (spit "/tmp/dictionary.txt" (apply str (interpose " " dict))
                     :encoding (:Encoding params))
               (let [results (sh "/home/josh/research/retrospect/helpers/words/bakeoff-scorer.pl"
                                 "/tmp/dictionary.txt" "/tmp/truewords.txt" "/tmp/history.txt")
                     prec (Double/parseDouble
                           (second (re-find #"TOTAL TEST WORDS PRECISION:\s+(\d\.\d\d\d)"
                                            (:out results))))
                     recall (Double/parseDouble
                             (second (re-find #"TOTAL TRUE WORDS RECALL:\s+(\d\.\d\d\d)"
                                              (:out results))))
                     f-score (Double/parseDouble
                              (second (re-find #"F MEASURE:\s+(\d\.\d\d\d)"
                                               (:out results))))
                     oov-recall (Double/parseDouble
                                 (second (re-find #"OOV Recall Rate:\s+(\d\.\d\d\d)"
                                                  (:out results))))]
                 [prec recall f-score oov-recall]))
             (catch Exception _ [-1.0 -1.0 -1.0 -1.0 -1.0]))]
    {:Prec prec
     :Recall recall
     :FScore f-score
     :OOVRecall oov-recall
     :LearnedCount (count learned)
     :LearnedCorrect (if (empty? learned) 100.0
                         (* 100.0 (/ (count (filter #(dict (first (:words %)))
                                                    learned))
                                     (count learned))))}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:LearnedCount :LearnedCorrect
                     :Prec :Recall :FScore :OOVRecall])))
