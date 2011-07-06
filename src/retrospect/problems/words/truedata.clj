(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:use [retrospect.random]))

(defn generate-truedata
  [datadir params]
  (let [start (my-rand-int 10000) 
        truedata-all (str/split (slurp (str datadir "/words/truedata.txt")) #" ")
        ambiguous (drop start (seq (slurp (str datadir "/words/ambiguous.txt")))) 
        [td prefix] (loop [td truedata-all
                           i 0]
                      (cond (= i start) [td []] 
                            (> i start) [td (vec (take (- i start) ambiguous))]
                            :else (recur (rest td) (+ i (count (first td))))))
        chop (inc (- (count prefix) (:StepsBetween params)))
        am (drop chop ambiguous)
        pre (drop chop prefix)]
    (with-meta (zipmap (range (count am)) am) {:words td :prefix pre})))

