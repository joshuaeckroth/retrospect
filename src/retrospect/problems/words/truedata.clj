(ns retrospect.problems.words.truedata
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [loom.graph :only [weighted-digraph weight add-edges edges]])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn find-inner-words
  [word dictionary]
  (if (empty? word) []
      (let [ws (filter dictionary (map (fn [i] (apply str (take i word)))
                                       (range 1 (inc (count word)))))]
        (concat
         (if (dictionary word) [[word]] [])
         (mapcat (fn [w] (let [rest-word (apply str (drop (count w) word))
                               rest-inner-words (find-inner-words rest-word dictionary)]
                           (map #(concat [w] %) rest-inner-words)))
                 ws)))))

(defn generate-truedata
  []
  (profile
   ;; attached a space at the end of each sentence to facilitate
   ;; sensor hyps that have pairs of symbols
   (let [sentences (map (fn [sent] (concat (filter not-empty (str/split sent #"[\s　]+"))
                                           [" "]))
                        (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                        @datadir (:Dataset params))
                                                :encoding "utf-8")))
         [training test2] (map vec (split-at (int (* (/ (:Knowledge params) 100.0)
                                                     (count sentences)))
                                             (my-shuffle sentences)))
         test (if (:ShortFirst params) (sort-by count test2) test2)
         test-dict (set (apply concat test))
         training-dict (set (apply concat training))
         dict-no-comps (when (:NoComposites params)
                         (loop [dict training-dict]
                           (let [composites
                                 (filter second (map #(find-inner-words % dict) dict))]
                             (if (empty? composites) dict
                                 (recur (apply disj dict
                                               (map #(apply str (first %)) composites)))))))
         ambiguous (map #(apply str %) test)
         ambiguous-training (map #(apply str %) training)]
     (when (:NoComposites params) (println "size dict" (count training-dict)
                                           "size dict no comp" (count dict-no-comps)))
     {:training {:test (zipmap (range (count ambiguous-training)) ambiguous-training)
                 :test-sentences training
                 :test-dict (if (:NoComposites params) dict-no-comps training-dict)}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-dict test-dict})))
