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
        dict (set (filter #(and (>= (count %) (:MinWordLength params))
                                (<= (count %) (:MaxLearnLength params)))
                          (str/split-lines (slurp (format "%s/words/%s/dictionary.txt"
                                                          @datadir (:Dataset params))
                                                  :encoding (:Encoding params)))))
        truedata-all (filter #(and (>= (count %) (:MinWordLength params))
                                   (<= (count %) (:MaxLearnLength params)))
                             (str/split (slurp (format "%s/words/%s/test.txt"
                                                       @datadir (:Dataset params))
                                               :encoding (:Encoding params)) #" "))
        truedata-all-noisy (map #(add-noise % sensor-noise) truedata-all)
        start (my-rand-int (- (reduce + (map count truedata-all)) (:Steps params)))
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
       :prefix pre :prefix-noisy pre-noisy
       :word-starts (get-trueword-starts prefix td)})))
