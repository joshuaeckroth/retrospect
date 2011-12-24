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
   the word, a sensor-noise chance for each letter of having a random
   replacement."
  [word sensor-noise]
  (if (< (my-rand) sensor-noise)
    (apply str (map #(if (< (my-rand) sensor-noise) (rand-char) %) (seq word)))
    word))

(defn generate-truedata
  []
  (let [sensor-noise (double (/ (:SensorNoise params) 100.0))
        dict (set (filter #(>= (count %) (:MinWordLength params))
                          (str/split-lines (slurp (str @datadir "/words/dictionary.txt")))))
        truedata-all (filter #(>= (count %) (:MinWordLength params))
                             (str/split (slurp (str @datadir "/words/truedata.txt")) #" "))
        truedata-all-noisy (map #(add-noise % sensor-noise) truedata-all)
        start (my-rand-int (reduce + (map count truedata-all)))
        ambiguous (take (:Steps params) (drop start (apply str truedata-all)))
        ambiguous-noisy (take (:Steps params) (drop start (apply str truedata-all-noisy)))
        [td prefix] (loop [td truedata-all
                           i 0]
                      (cond (= i start) [(take (:Steps params) td) []] 
                            (> i start) [(take (:Steps params) td)
                                         (vec (take (- i start) ambiguous))]
                            :else (recur (rest td) (+ i (count (first td))))))
        chop (inc (- (count prefix) (:StepsBetween params)))
        pre (drop chop prefix)
        am (drop chop ambiguous)
        am-noisy (drop chop ambiguous-noisy)
        pre-noisy (take (count pre) am-noisy)]
    (with-meta (zipmap (range (count am-noisy)) am-noisy)
      {:dictionary dict :words td :ambiguous am :ambiguous-noisy am-noisy
       :prefix pre :prefix-noisy pre-noisy})))
