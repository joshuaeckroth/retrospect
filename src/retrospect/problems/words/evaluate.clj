(ns retrospect.problems.words.evaluate
  (:import (misc LevenshteinDistance))
  (:use [clojure.contrib.map-utils :only [deep-merge-with]]))

(defn calc-ld
 [seq1 seq2]
 (LevenshteinDistance/ld (into-array String seq1) (into-array String seq2)))

(defn get-truewords
  [truedata time]
  (loop [ws []
         ws-count 0
         td (:words (meta truedata))]
    (let [c (+ ws-count (count (first td)))]
      (if (< time c) ws 
        (recur (conj ws (first td)) c (rest td))))))

(defn evaluate
  [ep-state results prev-ep sensors truedata params]
  (let [time (:time ep-state)
        truewords (get-truewords truedata (:time ep-state))]
    {:LD (calc-ld (:history (:problem-data ep-state)) truewords)}))

(defn avg-with-prior
  [results key val]
  (let [c (count results)]
    (cond (= c 0) val
          (= 0.0 val) (key (last results))
          :else (double (/ (+ (* c (key (last results))) val) (inc c))))))

(defn evaluate-meta
  [ep-state meta-ep-state meta-accepted-type results truedata params]
  (let [history (:history (:problem-data ep-state))
        history-meta (:history (:problem-data meta-ep-state))
        truewords (get-truewords truedata (:time ep-state))
        ld (calc-ld history truewords)
        ld-meta (calc-ld history-meta truewords)]
    {:AvgMetaDiffLD 
     (avg-with-prior results (keyword (format "%s%s" (name meta-accepted-type) "AvgMetaDiffLD"))
                     (- ld-meta ld))}))

(defn calc-percent-increase
  [k m b]
  (if (= 0 (k b)) 0.0
    (double (* 100.0 (/ (- (k m) (k b)) (k b))))))

(defn calc-ratio
  [k m b]
  (if (= 0 (k b)) 0.0
    (double (/ (k m) (k b)))))

(defn evaluate-comparative
  [params [m b]]
  {:MetaLD (:LD m)
   :BaseLD (:LD b)
   :RatioLD (calc-ratio :LD m b)
   :IncreaseLD (calc-percent-increase :LD m b)
   :MaxModelGrams (:MaxModelGrams params)})
