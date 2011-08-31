(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str])
  (:use [retrospect.random]))

(defn rand-char
  []
  (char (+ 97 (my-rand-int 26))))

(defn add-noise
  "Add three types of noise, with a sensor-noise/3 chance of each of
   them for every letter. Noise types: character deletions, additions,
   and replacements. The output of this function is a vector of
   2-element vectors: the first element is a (possibly noisy)
   character, the second element is the truedata index. Having the
   truedata index is necessary to reconstruct how much truedata (even
   if noisy) was actually presented after some number of steps."
  [letters sensor-noise]
  (loop [i 0 result []]
    (if (= i (count letters)) result
        (let [r (my-rand)]
          (cond (< r (/ sensor-noise 3.0))
                ;; deletion
                (recur (inc i) result)
                
                (< r (* 2.0 (/ sensor-noise 3.0)))
                ;; addition
                (recur (inc i) (conj result [(nth letters i) i] [(rand-char) i]))
                
                (< r sensor-noise)
                ;; replacement
                (recur (inc i) (conj result [(rand-char) i]))
                
                :else
                ;; no noise
                (recur (inc i) (conj result [(nth letters i) i])))))))

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
        pre (drop chop prefix)
        am-noisy (take (inc (:Steps params))
                       (add-noise am (double (/ (:SensorNoise params) 100.0))))]
    (with-meta (zipmap (range (count am-noisy)) (map first am-noisy))
      {:words td :prefix pre :noisy am-noisy})))

