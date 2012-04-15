(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn rand-char
  []
  (char (+ 97 (my-rand-int 26))))

(defn add-noise
  "Add \"replacement\" noise, with a sensor-noise chance (between 0.0
   and 1.0) of having any noise in the word, and if noise will be in
   the word, a sensor-noise/2 chance for each letter of having a random
   replacement."
  [word sensor-noise]
  (if (< (my-rand) sensor-noise)
    (apply str (map #(if (< (my-rand) (/ sensor-noise 2)) (rand-char) %) (seq word)))
    word))

(defn generate-truedata
  []
  (let [sensor-noise (double (/ (:SensorNoise params) 100.0))
        sentences (map (fn [sent] (filter not-empty (str/split sent #"\s+")))
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
    {:training [training training-dict training-symbols]
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test-shuffled
     :test-dict test-dict}))
