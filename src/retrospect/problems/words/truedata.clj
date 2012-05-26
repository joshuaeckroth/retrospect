(ns retrospect.problems.words.truedata
  (:import (java.util.regex Pattern))
  (:import (org.arabidopsis.ahocorasick AhoCorasick))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [loom.graph :only [weighted-digraph weight add-edges edges]])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn add-to-in-word-bigram
  [iwb sent]
  (reduce
   (fn [m w]
     (reduce (fn [m2 [c1 c2]]
               (let [p (get m2 [c1 c2] 0)
                     p-in (get m2 [:in c2] 0)
                     p-out (get m2 [c1 :out] 0)]
                 (-> m2
                     (assoc [c1 c2] (inc p))
                     (assoc [:in c2] (inc p-in))
                     (assoc [c1 :out] (inc p-out)))))
             m (conj (partition 2 1 (seq w))
                     ["start" (first w)]
                     [(last w) "end"])))
   iwb sent))

(defn add-to-wtc
  [wtc sent]
  (reduce (fn [m [w1 w2]]
            (let [p (get m [(last w1) (first w2)] 0)]
              (assoc m [(last w1) (first w2)] (inc p))))
          wtc (partition 2 1 sent)))

(defn add-to-unigram-model
  [ugm sent]
  (reduce (fn [m w] (let [p (get m w 0)]
                      (assoc m w (inc p))))
          ugm sent))

(defn add-to-bigram-model
  [bgm sent]
  (reduce (fn [m [w1 w2]]
            (let [p (get m [w1 w2] 0)]
              (assoc m [w1 w2] (inc p))))
          bgm (partition 2 1 sent)))

(comment
  [iwb wtc ugm bgm]
  (prof :iwb-wtc-ugm
        (reduce (fn [[iwb wtc ugm bgm] sent]
                  [(add-to-in-word-bigram iwb sent)
                   (add-to-wtc wtc sent)
                   (add-to-unigram-model ugm sent)
                   (add-to-bigram-model bgm sent)])
                [{} {} {} {}] training))
  )

(defn generate-truedata
  []
  (profile
   ;; attached a space at the front and end of each sentence to
   ;; facilitate sensor hyps that have pairs of symbols
   (let [sentences (map (fn [sent] (concat (filter not-empty (str/split sent #"[\s　]+"))
                                           [" "]))
                        (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                        @datadir (:Dataset params))
                                                :encoding "utf-8")))
         [training test2] (split-at (int (* (/ (:Knowledge params) 100)
                                            (count sentences)))
                                    (my-shuffle sentences))
         test (if (:ShortFirst params) (sort-by count test2) test2)
         [training-dict test-dict] (map (fn [sents] (set (apply concat sents)))
                                        [training test])
         training-symbols (set (apply concat training-dict))
         ;; TODO: handle noise
         ambiguous (map #(apply str %) test)
         ambiguous-training (map #(apply str %) training)
         dict-tree (let [tree (AhoCorasick.)]
                     (doseq [w training-dict]
                       (.add tree (.getBytes w) w))
                     (.prepare tree)
                     tree)]
     {:training {:test (zipmap (range (count ambiguous-training)) ambiguous-training)
                 :test-sentences training
                 :test-dict training-dict
                 :sentences training
                 :dictionary training-dict
                 :orig-dictionary training-dict
                 :symbols training-symbols
                 :dictionary-tree dict-tree}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-dict test-dict})))
