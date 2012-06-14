(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn extract-tags-word
  [word]
  (map (fn [i] (let [sym (str (nth word i))]
              (cond (and (= 0 i) (= (dec (count word)) i))
                    [sym :Only]
                    (= 0 i)
                    [sym :Start]
                    (= (dec (count word)) i)
                    [sym :End]
                    :else
                    [sym :Middle])))
     (range (count word))))

(defn extract-tags
  [sents]
  (vec (map (fn [sent] (vec (mapcat extract-tags-word sent))) sents)))

(defn generate-truedata
  []
  (profile
   (let [sentences (doall (map (fn [sent] (doall (filter not-empty (str/split sent #"[\s　]+"))))
                             (str/split-lines
                              (slurp (format "%s/words/%s.utf8" @datadir (:Dataset params))
                                     :encoding "utf-8"))))
         [training2 test2] (map vec (split-at (int (* (/ (:Knowledge params) 100.0)
                                                    (count sentences)))
                                            (my-shuffle sentences)))
         test (if (:ShortFirst params)
                (vec (take (:Steps params) (sort-by count test2)))
                (vec (take (:Steps params) test2)))
         test-tags (extract-tags test)
         training (if (:TestIsTraining params) test training2)
         training-tags (extract-tags training)
         training-dict (set (apply concat training))
         ambiguous (map #(apply str %) test)
         ambiguous-training (map #(apply str %) training)]
     (println (take 3 training))
     (println (take 3 training-tags))
     {:training {:test (zipmap (range (count ambiguous-training)) ambiguous-training)
                 :test-sentences training
                 :test-tags training-tags
                 :dict training-dict}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-tags test-tags})))
