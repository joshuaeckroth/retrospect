(ns retrospect.reason.abduction.problems.classify.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only [cur-ep ep-path]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]]))

(defn true-hyp?
  [truedata time-now hyp]
  (cond (= :word (:type hyp)) true
        (= :kb (:type hyp)) true
        :else
        ((get-in truedata [:cats (:docid hyp)]) (:cat hyp))))

(defn chosen-cats
  [doccats workspace]
  (let [dochyps (group-by :docid (map #(lookup-hyp workspace %)
                                    (:category (:accepted workspace))))]
    (reduce (fn [m [docid hyps]]
         (let [prior-cats (get m docid)]
           (if prior-cats (update-in m [docid] set/union (set (map :cat hyps)))
               (assoc m docid (set (map :cat hyps))))))
       doccats (seq dochyps))))

(defn evaluate
  [truedata est]
  (let [eps (rest (ep-path est))
        time-now (:time (last eps))
        doccats (reduce chosen-cats {} (map :workspace eps))
        [tp fp fn] (loop [tp 0 fp 0 fn 0
                          docids (map #(first (get (:test truedata) %))
                                    (range time-now))]
                     (if (empty? docids) [tp fp fn]
                         (let [true-cats (get (:cats truedata) (first docids))
                               chosen-cats (get doccats (first docids) #{})]
                           (recur (+ tp (count (set/intersection true-cats chosen-cats)))
                                  (+ fp (count (set/difference chosen-cats true-cats)))
                                  (+ fn (count (set/difference true-cats chosen-cats)))
                                  (rest docids)))))
        prec (if (= 0 (+ tp fp)) 0.0 (/ (double tp) (double (+ tp fp))))
        recall (if (= 0 (+ tp fn)) 0.0 (/ (double tp) (double (+ tp fn))))
        fscore (if (= 0.0 (+ prec recall)) 0.0 (/ (* 2.0 prec recall) (+ prec recall)))]
    {:TP tp :FP fp :FN fn
     :Prec prec :Recall recall :FScore fscore}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  {})

(defn training-stats
  [workspace false-accepted unexplained truedata time-now cycle])

(defn stats
  [truedata ors time-now])
