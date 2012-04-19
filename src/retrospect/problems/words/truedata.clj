(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
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
        ambiguous (map #(apply str %) test-shuffled)]
    (comment
      (println "Occurrences of OOV:")
      (println (sort (vals (reduce (fn [m w] (if (nil? (get m w)) (assoc m w 1)
                                                 (update-in m [w] inc)))
                                   {}
                                   (filter (fn [w] (not (training-dict w)))
                                           (apply concat test-shuffled)))))))
    {:training {:sentences training :dictionary training-dict :symbols training-symbols}
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test-shuffled
     :test-dict test-dict}))
