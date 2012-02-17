(ns retrospect.reason.abduction.problems.words.evaluate
  (:use [clojure.java.shell :only [sh]])
  (:use [retrospect.reason.abduction.evaluate :only [calc-increase]])
  (:use [retrospect.problems.words.symbols])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata pdata time hyp]
  false)

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
  [accepted rejected time-now sensors truedata]
  (let [believed (filter #(not (re-matches punctuation-regex %))
                         (get-history accepted))
        learned (filter #(= :learned-word (:subtype %))
                        (get accepted :word))
        sentence (filter #(not (re-matches punctuation-regex %))
                         (nth (:test-sentences truedata) (dec time-now)))
        [prec recall f-score oov-rate oov-recall iv-recall]
        (try (do
               (spit "/tmp/truewords.txt" (apply str (interpose " " sentence))
                     :encoding "utf-8")
               (spit "/tmp/history.txt" (apply str (interpose " " believed))
                     :encoding "utf-8")
               (spit "/tmp/dictionary.txt"
                     (apply str (interpose "\n" (second (:training truedata))))
                     :encoding "utf-8")
               (let [results (sh (format "%s/words/score" @datadir)
                                 "/tmp/dictionary.txt" "/tmp/truewords.txt" "/tmp/history.txt")
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
     :IVRecall iv-recall
     :LearnedCount (count learned)
     :LearnedCorrect (if (empty? learned) 100.0
                         (* 100.0 (/ (count (filter #((:test-dict truedata) (:word %))
                                                    learned))
                                     (count learned))))}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:LearnedCount :LearnedCorrect
                     :Prec :Recall :FScore :OOVRecall])))
