(ns retrospect.reason.abduction.problems.words.evaluate
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str])
  (:use [clojure.contrib.io :only [file]])
  (:use [clojure.contrib.seq-utils :only [find-first]])
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate :only [calc-increase avg]])
  (:use [retrospect.epistemicstates :only [cur-ep ep-path]])
  (:use [retrospect.reason.abduction.workspace :only [hyp-conf lookup-hyp]])
  (:use [loom.graph :only [weight]])
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.logging])
  (:use [retrospect.state]))

(defn true-hyp?
  [truedata time-now hyp]
  (prof :true-hyp
        (or (= :sensor (:type hyp))
            (= :kb (:type hyp))
            (= :transition (:type hyp))
            (let [sentence (nth (:test-sentences truedata) (dec time-now))
                  start-pos (or (first (:pos-seq hyp)) (inc (:trans-pos hyp)))
                  end-pos (or (last (:pos-seq hyp)) (inc (:trans-pos hyp)))
                  true-words (loop [i 0 ws [] sent sentence]
                               (cond (empty? sent) ws
                                     (> i end-pos) ws
                                     (< i start-pos)
                                     (recur (+ i (count (first sent)))
                                            ws (rest sent))
                                     :else
                                     (recur (+ i (count (first sent)))
                                            (conj ws (first sent))
                                            (rest sent))))]
              (cond (= :word (:type hyp)) (= [(:word hyp)] true-words)
                    (= :merge (:type hyp)) (empty? true-words)
                    ;; leave if and true/false because not-empty returns the first item
                    (= :split (:type hyp)) (if (not-empty true-words) true false))))))

(defn run-scorer
  [sentences believed train-dict]
  (try (do
         (spit (format "/tmp/truth-%d.txt" (:simulation params))
               (str/join "\n" (map #(str/join " " %) sentences))
               :encoding "utf-8")
         (spit (format "/tmp/believed-%d.txt" (:simulation params))
               (str/join "\n" (map (fn [s] (if (empty? s) "_" s))
                                   (map #(str/join " " %) believed)))
               :encoding "utf-8")
         (spit (format "/tmp/dictionary-%d.txt" (:simulation params))
               (str/join "\n" (sort train-dict))
               :encoding "utf-8")
         (let [results (sh (format "%s/words/score" @datadir)
                           (format "/tmp/dictionary-%d.txt" (:simulation params))
                           (format "/tmp/truth-%d.txt" (:simulation params))
                           (format "/tmp/believed-%d.txt" (:simulation params)))
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
           #_(println (format "prec: %.2f, recall: %.2f, f-score: %.2f, oov-recall: %.2f, oov-rate: %.2f"
                            prec recall f-score oov-recall oov-rate))
           [prec recall f-score oov-rate oov-recall iv-recall]))
       (catch Exception e (do (log e) [-1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0]))))

(defn get-words
  [lookup-hyp ambiguous accepted unexplained]
  (let [kb (first (get accepted :kb))
        cuts (sort (set (concat (map :trans-pos (map lookup-hyp (get accepted :split)))
                                (map (comp dec first :pos-seq)
                                     (map lookup-hyp (get accepted :word)))
                                (map (comp last :pos-seq)
                                     (map lookup-hyp (get accepted :word)))
                                (if (= "merge" (:DefaultMergeSplit params)) []
                                    (map :trans-pos (filter #(= :transition (:type %))
                                                            unexplained))))))]
    (loop [amb (vec ambiguous)
           cs (filter #(>= % 0) cuts)
           i 0
           words []]
      (cond (empty? amb) words
            (empty? cs) (conj words (apply str amb))
            :else
            (let [c (inc (first cs))]
              (recur (vec (drop (- c i) amb))
                     (rest cs)
                     c
                     (conj words (apply str (subvec amb 0 (- c i))))))))))

(defn evaluate
  [truedata est]
  (if (or (and (not training?) (not @batch))
          (and (not training?) (= (:Steps params) (:time (cur-ep est)))))
    (let [eps (rest (ep-path est))
          time-now (:time (last eps))
          believed (map (fn [ep] (get-words
                                  (partial lookup-hyp (:workspace ep))
                                  (get (:test truedata) (dec (:time ep)))
                                  (:accepted (:workspace ep))
                                  (:unexplained (:log (:workspace ep)))))
                        eps)
          sentences (map (fn [i] (nth (:test-sentences truedata) i)) (range time-now))
          [prec recall f-score oov-rate oov-recall iv-recall]
          (run-scorer sentences believed (:test-dict (:training truedata)))]
      {:Prec prec
       :Recall recall
       :FScore f-score
       :OOVRate oov-rate
       :OOVRecall oov-recall
       :IVRecall iv-recall})
    {:Prec -1.0
     :Recall -1.0
     :FScore -1.0
     :OOVRate -1.0
     :OOVRecall -1.0
     :IVRecall -1.0}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:LearnedCount :LearnedCorrect
                     :Prec :Recall :FScore :OOVRecall])))

(defn find-oov
  [truedata time-now]
  (let [sentence (nth (:test-sentences truedata) (dec time-now))
        oov (set (filter #(not ((:test-dict (:training truedata)) %)) sentence))]
    (reduce (fn [m w] (assoc m w (map (fn [i] (reduce + (map count (take i sentence))))
                                      (filter #(= w (nth sentence %)) (range (count sentence))))))
            {} oov)))

(defn stats
  [truedata ors time-now]
  (comment (let [ws (:workspace (cur-ep (:est ors)))
                 kb (first (get (:hypotheses ws) :kb))
                 sentence (nth (:test-sentences truedata) (dec time-now))
                 oov (find-oov truedata time-now)
                 newsyms (find-new-symbols truedata time-now)
                 words-stats-file (file (format "%s/words/words-stats.csv" @datadir))
                 word-seqs-stats-file (file (format "%s/words/word-seqs-stats.csv" @datadir))
                 sensors-stats-file (file (format "%s/words/sensors-stats.csv" @datadir))
                 oov-stats-file (file (format "%s/words/oov-stats.csv" @datadir))
                 sentence-stats-file (file (format "%s/words/sentence-stats.csv" @datadir))
                 learn-stats-file (file (format "%s/words/learn-stats.csv" @datadir))
                 [prec recall f-score oov-rate oov-recall iv-recall]
                 (run-scorer [sentence] [(get-words (:accepted ws))] truedata)]
             (when (not (. sentence-stats-file exists))
               (with-open [r (java.io.FileWriter. sentence-stats-file)]
                 (.write r "wc,oov,newsym,doubt,coverage,noexp,unexp,wordhyps,prec,recall,fscore,oovrate,oovrecall,ivrecall\n")))
             (with-open [r (java.io.FileWriter. sentence-stats-file true)]
               (.write r (format "%d,%d,%d,%f,%f,%d,%d,%d,%f,%f,%f,%f,%f,%f\n"
                                 (count sentence) (count oov) (count newsyms)
                                 (:doubt ws) (:coverage ws)
                                 (count (:no-explainers (:log ws)))
                                 (count (:unexplained (:log ws)))
                                 (count (:word (:hypotheses ws)))
                                 prec recall f-score oov-rate oov-recall iv-recall)))
             (when (not (. sensors-stats-file exists))
               (with-open [r (java.io.FileWriter. sensors-stats-file)]
                 (.write r "explainers,oov,unexp\n")))
             (with-open [r (java.io.FileWriter. sensors-stats-file true)]
               (doseq [sensor-hyp (:sensor (:hypotheses ws))]
                 (let [explainers (count (get (:explainers ws) sensor-hyp))
                       unexp? ((:needs-explainer ws) sensor-hyp)]
                   (.write r (format "%d,%s,%s\n"
                                     explainers
                                     ;; find out if the sensor hyp has an oov word overlapping it
                                     (if (some (fn [[w positions]]
                                                 (some #(= % (:pos sensor-hyp))
                                                       (mapcat (fn [pos] (range pos (+ pos (count w))))
                                                               positions)))
                                               (seq oov))
                                       "\"T\"" "\"F\"")
                                     (if unexp? "\"T\"" "\"F\""))))))
             (when (not (. oov-stats-file exists))
               (with-open [r (java.io.FileWriter. oov-stats-file)]
                 (.write r "length,noexpSyms,firstNoexpPos,explainers,avgExpApriori,avgExpConf,occur,occurSent\n")))
             (with-open [r (java.io.FileWriter. oov-stats-file true)]
               (doseq [w (keys oov)]
                 (let [sensor-hyps (sort-by :pos (set (mapcat
                                                       (fn [pos]
                                                         (map (fn [i] (find-first #(= i (:pos %))
                                                                                  (:sensor (:hypotheses ws))))
                                                              (range pos (+ pos (count w)))))
                                                       (get oov w))))
                       explainers (set (mapcat (fn [sh] (get (:explainers ws) sh)) sensor-hyps))
                       noexps (filter (fn [sh] (empty? (get (:explainers ws) sh))) sensor-hyps)
                       first-unexp-pos (if (empty? noexps) -1 (- (:pos (first noexps)) (:pos (first sensor-hyps))))
                       aprioris (map :apriori explainers)
                       confs (map #(hyp-conf ws %) explainers)]
                   (.write r (format "%d,%d,%d,%d,%f,%f,%d,%d\n"
                                     (count w)
                                     (count noexps)
                                     first-unexp-pos
                                     (count explainers)
                                     (if (empty? explainers) 0.0 (/ (reduce + aprioris) (double (count aprioris))))
                                     (if (empty? explainers) 0.0 (/ (reduce + confs) (double (count confs))))
                                     (get (:test-word-freq truedata) w)
                                     (get (frequencies (nth (:test-sentences truedata) (dec time-now))) w))))))
             (when (not (. words-stats-file exists))
               (with-open [r (java.io.FileWriter. words-stats-file)]
                 (.write r "tf,delta,explainers,oov,apriori,conf,conflicts\n")))
             (with-open [r (java.io.FileWriter. words-stats-file true)]
               (doseq [word-hyp (filter #(= :word (:subtype %)) (:word (:hypotheses ws)))]
                 (let [b (find-first #(= word-hyp (:best %)) (:best (:log ws)))]
                   (.write r (format "%s,%f,%d,%s,%f,%f,%d\n"
                                     (if (true-hyp? truedata time-now word-hyp) "\"T\"" "\"F\"")
                                     (cond (nil? b) -1.0 (:delta b) (:delta b) :else 1.0)
                                     (count (get (:explainers ws) word-hyp))
                                     ;; find out if the word hyp has an oov word overlapping it
                                     ;; (this should indicate the word hyp is false, btw)
                                     (if (some (fn [[w positions]]
                                                 (some #(not-empty (set/intersection % (set (:pos-seq word-hyp))))
                                                       (map (fn [pos] (set (range pos (+ pos (count w)))))
                                                            positions)))
                                               (seq oov))
                                       "\"T\"" "\"F\"")
                                     (:apriori word-hyp)
                                     (hyp-conf ws word-hyp)
                                     (count (find-conflicts ws word-hyp)))))))
             (when (not (. word-seqs-stats-file exists))
               (with-open [r (java.io.FileWriter. word-seqs-stats-file)]
                 (.write r "tf,delta,explains,oov,apriori,conf,conflicts\n")))
             (with-open [r (java.io.FileWriter. word-seqs-stats-file true)]
               (doseq [word-seq-hyp (:word-seq (:hypotheses ws))]
                 (let [b (find-first #(= word-seq-hyp (:best %)) (:best (:log ws)))]
                   (.write r (format "%s,%f,%d,%s,%f,%f,%d\n"
                                     (if (true-hyp? truedata time-now word-seq-hyp) "\"T\"" "\"F\"")
                                     (cond (nil? b) -1.0 (:delta b) (:delta b) :else 1.0)
                                     (count (:explains word-seq-hyp))
                                     ;; find out if the word-seq hyp has an oov word overlapping it
                                     ;; (this should indicate the word-seq hyp is false, btw)
                                     (if (some (fn [[w positions]]
                                                 (some #(not-empty
                                                         (set/intersection 
                                                          % (set (apply concat (:pos-seqs word-seq-hyp)))))
                                                       (map (fn [pos] (set (range pos (+ pos (count w)))))
                                                            positions)))
                                               (seq oov))
                                       "\"T\"" "\"F\"")
                                     (:apriori word-seq-hyp)
                                     (hyp-conf ws word-seq-hyp)
                                     (count (find-conflicts ws word-seq-hyp)))))))
             (when (not (. learn-stats-file exists))
               (with-open [r (java.io.FileWriter. learn-stats-file)]
                 (.write r "tf,delta,explains,oov,apriori,conf,conflicts,gauss,mult,opp,avg,min,max\n")))
             (with-open [r (java.io.FileWriter. learn-stats-file true)]
               (doseq [learn-hyp (filter #(= :learned-word (:subtype %)) (:word (:hypotheses ws)))]
                 (let [b (find-first #(= learn-hyp (:best %)) (:best (:log ws)))]
                   (.write r (format "%s,%f,%d,%s,%f,%f,%d,%f,%f,%f,%f,%f,%f\n"
                                     (if (true-hyp? truedata time-now learn-hyp) "\"T\"" "\"F\"")
                                     (cond (nil? b) -1.0 (:delta b) (:delta b) :else 1.0)
                                     (count (:explains learn-hyp))
                                     ;; find out if the word-seq hyp has an oov word overlapping it
                                     ;; (this should indicate the word-seq hyp is false, btw)
                                     (if (some (fn [[w positions]]
                                                 (some #(not-empty
                                                         (set/intersection 
                                                          % (set (:pos-seq learn-hyp))))
                                                       (map (fn [pos] (set (range pos (+ pos (count w)))))
                                                            positions)))
                                               (seq oov))
                                       "\"T\"" "\"F\"")
                                     (:apriori learn-hyp)
                                     (hyp-conf ws learn-hyp)
                                     (count (find-conflicts ws learn-hyp))
                                     (:gauss learn-hyp)
                                     (:mult (:tendencies-map learn-hyp))
                                     (:opp (:tendencies-map learn-hyp))
                                     (:avg (:tendencies-map learn-hyp))
                                     (:min (:tendencies-map learn-hyp))
                                     (:max (:tendencies-map learn-hyp))))))))))
