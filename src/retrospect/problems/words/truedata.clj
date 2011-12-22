(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn rand-char
  []
  (char (+ 97 (my-rand-int 26))))

(defn add-noise
  "Add \"replacement\" noise, with a sensor-noise chance (between 0.0
   and 1.0) of a random replacement for every letter."
  [letters sensor-noise]
  (map #(if (< (my-rand) sensor-noise) (rand-char) %) letters))

(defn generate-truedata
  []
  (let [dict (set (str/split-lines (slurp (str @datadir "/words/dictionary.txt"))))
        truedata-all (str/split (slurp (str @datadir "/words/truedata.txt")) #" ")
        start (my-rand-int (reduce + 0 (map count truedata-all)))
        ambiguous (take (:Steps params)
                        (drop start (seq (slurp (str @datadir "/words/ambiguous.txt"))))) 
        [td prefix] (loop [td truedata-all
                           i 0]
                      (cond (= i start) [(take (:Steps params) td) []] 
                            (> i start) [(take (:Steps params) td)
                                         (vec (take (- i start) ambiguous))]
                            :else (recur (rest td) (+ i (count (first td))))))
        chop (inc (- (count prefix) (:StepsBetween params)))
        sensor-noise (double (/ (:SensorNoise params) 100.0))
        pre (drop chop prefix)
        am (drop chop ambiguous)
        am-noisy (add-noise am sensor-noise)
        pre-noisy (take (count pre) am-noisy)]
    (with-meta (zipmap (range (count am-noisy)) am-noisy)
      {:dictionary dict :words td :ambiguous am :ambiguous-noisy am-noisy
       :prefix pre :prefix-noisy pre-noisy})))
