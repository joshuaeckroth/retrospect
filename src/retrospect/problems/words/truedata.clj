(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:use [retrospect.profile :only [prof profile]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn find-inner-words
  [word dictionary]
  (prof :find-inner-words
        (if (empty? word) []
            (let [ws (filter dictionary (map (fn [i] (apply str (take i word)))
                                             (range 1 (inc (count word)))))]
              (concat
               (if (dictionary word) [[word]] [])
               (mapcat (fn [w] (let [rest-word (apply str (drop (count w) word))
                                     rest-inner-words (find-inner-words rest-word dictionary)]
                                 (map #(concat [w] %) rest-inner-words)))
                       ws))))))

(defn find-true-breaks
  [sentences]
  (prof :find-true-breaks
        (vec (map (fn [sent]
                    (loop [i 0
                           breaks #{}
                           s sent]
                      (if (empty? s)
                        (conj breaks i)
                        (recur (+ i (count (first s)))
                               (conj breaks i) (rest s)))))
                  sentences))))

(defn generate-truedata
  []
  (profile
   (let [sentences (doall (map (fn [sent]
                               (doall (concat (filter not-empty
                                                 (str/split sent #"[\s　]+")))))
                             (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                        @datadir (:Dataset params))
                                                     :encoding "utf-8"))))
         [training2 test2] (map vec (split-at (int (* (/ (:Knowledge params) 100.0)
                                                    (count sentences)))
                                            (my-shuffle sentences)))
         test (if (:ShortFirst params)
                (vec (take (:Steps params) (sort-by count test2)))
                (vec (take (:Steps params) test2)))
         training (if (:TestIsTraining params) test training2)
         [training-breaks test-breaks] (map find-true-breaks [training test])
         test-dict (set (apply concat test))
         training-dict (set (concat (apply concat training)
                                    ;; add all characters, too
                                    (map str (apply concat (apply concat training)))))
         dict-no-comps (when (:NoComposites params)
                         (loop [dict training-dict]
                           (.write System/out (int \-))
                           (.flush System/out)
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
                 :test-breaks training-breaks
                 :test-dict (if (:NoComposites params) dict-no-comps training-dict)}
      :test (zipmap (range (count ambiguous)) ambiguous)
      :test-sentences test
      :test-breaks test-breaks
      :test-dict test-dict})))
