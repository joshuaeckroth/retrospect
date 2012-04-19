(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:use [loom.graph :only [weighted-digraph weight add-edges]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn generate-truedata
  []
  (let [sentences (map (fn [sent] (filter not-empty (str/split sent #"\s+")))
                       (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                       @datadir (:Dataset params))
                                               :encoding "utf-8")))
        [training test] (split-at (int (* 0.9 (count sentences))) sentences)
        test-shuffled (my-shuffle test)
        [training-dict test-dict] (map (fn [sents] (set (apply concat sents)))
                                       [training test-shuffled])
        training-symbols (set (apply concat training-dict))
        ;; TODO: handle noise
        ambiguous (map #(apply str %) test-shuffled)
        dtg (reduce (fn [g word] (reduce (fn [g2 e]
                                           (let [w (or (apply weight g e) 0)]
                                             (add-edges g2 (conj e (inc w)))))
                                         g (conj (map vec (partition 2 1 word))
                                                 ["start" (first word)]
                                                 [(last word) "end"])))
                    (weighted-digraph)
                    training-dict)
        wtc (set (mapcat (fn [sent] (map (fn [[w1 w2]] [(last w1) (first w2)])
                                         (partition 2 1 sent)))
                         training))]
    {:training {:sentences training :dictionary training-dict :symbols training-symbols
                :dtg dtg :wtc wtc}
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test-shuffled
     :test-dict test-dict}))
