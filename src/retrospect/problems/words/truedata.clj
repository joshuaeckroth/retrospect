(ns retrospect.problems.words.truedata
  (:import (java.util.regex Pattern))
  (:require [clojure.string :as str])
  (:use [clojure.contrib.string :only [substring?]])
  (:use [loom.graph :only [weighted-digraph weight add-edges edges]])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

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
         [training test2] (split-at (int (* (/ (:Knowledge params) 100)
                                            (count sentences)))
                                    (my-shuffle sentences))
         test (if (:ShortFirst params) (sort-by count test2) test2)
         test-dict (set (apply concat test))
         training-dict (set (apply concat training))
         ambiguous (map #(apply str %) test)
         ambiguous-training (map #(apply str %) training)]
     {:training {:test (zipmap (range (count ambiguous-training)) ambiguous-training)
                 :test-sentences training
                 :test-dict training-dict}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-dict test-dict})))
