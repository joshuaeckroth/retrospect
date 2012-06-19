(ns retrospect.problems.words.truedata
  (:import (java.io File))
  (:import (org.arabidopsis.ahocorasick AhoCorasick))
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [clojure.java.shell :only [sh]])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn crf-format-word
  [word]
  (apply str (map (fn [i] (let [sym (str (nth word i))]
                         (cond (and (= 0 i) (= (dec (count word)) i))
                               (format "%s\tO\n" sym)
                               (= 0 i)
                               (format "%s\tS\n" sym)
                               (= (dec (count word)) i)
                               (format "%s\tE\n" sym)
                               :else
                               (format "%s\tM\n" sym))))
                (range (count word)))))

(defn crf-format
  [sents]
  (str/join "\n" (map (fn [sent] (apply str (map crf-format-word sent))) sents)))

(defn extract-crf-scores
  [marginals]
  (let [tests (str/split marginals #"\n\n")]
    (vec (map (fn [test]
              (vec (map
                    (fn [[sym _ _ & tags]]
                      [sym (vec (reverse
                                 (sort-by second
                                          (map #(let [[tag score] (str/split % #"/")]
                                                [tag (Double/parseDouble score)]) tags))))])
                    (vec (map #(vec (str/split % #"\s+"))
                            ;; skip first line of each test, which gives
                            ;; overall confidence score
                            (rest (str/split-lines test)))))))
            tests))))

(defn extract-tags-word
  [word]
  (vec (map (fn [i] (let [sym (str (nth word i))]
                   (cond (and (= 0 i) (= (dec (count word)) i))
                         [sym "O"]
                         (= 0 i)
                         [sym "S"]
                         (= (dec (count word)) i)
                         [sym "E"]
                         :else
                         [sym "M"])))
          (range (count word)))))

(defn extract-tags
  [sents]
  (vec (map (fn [sent] (vec (mapcat extract-tags-word sent))) sents)))

(comment )

(defn generate-truedata
  []
  (profile
   (let [sentences (doall (map (fn [sent] (doall (filter not-empty (str/split sent #"[\s　]+"))))
                             (str/split-lines
                              (slurp (format "%s/words/%s.utf8" @datadir (:Dataset params))
                                     :encoding "utf-8"))))
         split-location (int (* (/ (:Knowledge params) 100.0) (count sentences)))
         [training2 test2] (map vec (split-at split-location (my-shuffle sentences)))
         test (if (:ShortFirst params)
                (vec (take (:Steps params) (sort-by count test2)))
                (vec (take (:Steps params) test2)))
         test-tags (extract-tags test)
         training (if (:TestIsTraining params) test training2)
         training-tags (extract-tags training)
         training-dict (set (apply concat training))
         test-dict (set (apply concat test))
         dict-tree (let [dict-tree (AhoCorasick.)]
                     (doseq [w (set/union training-dict test-dict)]
                       (.add dict-tree (.getBytes w) w))
                     (.prepare dict-tree)
                     dict-tree)
         scores (do
                  (when (or (not (.exists (File. (format "%s/words/%s-%d-%d.crf-input-training"
                                                    @datadir (:Dataset params)
                                                    (:Seed params) split-location))))
                            (not (.exists (File. (format "%s/words/%s-%d-%d.crf-input-test"
                                                    @datadir (:Dataset params)
                                                    (:Seed params) split-location))))
                            (not (.exists (File. (format "%s/words/%s-%d-%d.crf-model"
                                                    @datadir (:Dataset params)
                                                    (:Seed params) split-location))))
                            (not (.exists (File. (format "%s/words/%s-%d-%d.scores"
                                                    @datadir (:Dataset params)
                                                    (:Seed params) split-location)))))
                    ;; create input files
                    (spit (format "%s/words/%s-%d-%d.crf-input-training"
                             @datadir (:Dataset params) (:Seed params) split-location)
                          (crf-format training))
                    (spit (format "%s/words/%s-%d-%d.crf-input-test"
                             @datadir (:Dataset params) (:Seed params) split-location)
                          (crf-format test))
                    ;; train CRF
                    (sh "crf_learn" "-p" "4"
                        (format "%s/words/%s.crf-template" @datadir (:Dataset params))
                        (format "%s/words/%s-%d-%d.crf-input-training"
                           @datadir (:Dataset params) (:Seed params) split-location)
                        (format "%s/words/%s-%d-%d.crf-model"
                           @datadir (:Dataset params) (:Seed params) split-location))
                    ;; create scores file
                    (spit
                     (format "%s/words/%s-%d-%d.scores"
                        @datadir (:Dataset params) (:Seed params) split-location)
                     (:out
                      (sh "crf_test" "-v2" "-m"
                          (format "%s/words/%s-%d-%d.crf-model"
                             @datadir (:Dataset params) (:Seed params) split-location)
                          (format "%s/words/%s-%d-%d.crf-input-test"
                             @datadir (:Dataset params) (:Seed params) split-location)))))
                  (extract-crf-scores
                   (slurp (format "%s/words/%s-%d-%d.scores"
                             @datadir (:Dataset params) (:Seed params) split-location))))
         ambiguous (map #(apply str %) test)
         ambiguous-training (map #(apply str %) training)]
     {:training {:test (zipmap (range (count ambiguous-training)) ambiguous-training)
                 :test-sentences training
                 :test-tags training-tags
                 :scores scores
                 :dict training-dict
                 :dict-tree dict-tree}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-tags test-tags})))
