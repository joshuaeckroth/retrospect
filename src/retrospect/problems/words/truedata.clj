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

(defn get-trueword-starts
  [prefix words]
  (loop [ws []
         ws-count (count prefix)
         td words]
    (if (empty? td) ws
        (let [c (+ ws-count (count (first td)))]
          (if (< (:Steps params) c) ws 
              (recur (conj ws [(first td) ws-count]) c (rest td)))))))

(defn generate-truedata
  []
  (let [sensor-noise (double (/ (:SensorNoise params) 100.0))
        sentences (map (fn [sent] (filter not-empty (str/split sent #"\s+")))
                       (str/split-lines (slurp (format "%s/words/%s.utf8"
                                                       @datadir (:Dataset params))
                                               :encoding "utf-8")))
        [training test] (split-at (int (* 0.05 (count sentences)))
                                  (my-shuffle sentences))
        [training-dict test-dict] (map (fn [sents] (set (apply concat sents)))
                                       [training test])
        ;; TODO: handle noise
        ambiguous (map #(apply str %) test)]
    {:training [training training-dict]
     :test (zipmap (range (count ambiguous)) ambiguous)
     :test-sentences test
     :test-dict test-dict}))
