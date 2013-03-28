(ns retrospect.problems.classify.evaluate
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]]))

(defn true-hyp?
  [truedata time-now hyp]
  (cond (= :word (:type hyp)) true
        (= :kb (:type hyp)) true
        :else
        (let [true-cats (get-in truedata [:cats (:docid hyp)])]
          (and (every? true-cats (:categories hyp))
               (not-any? true-cats (:not-categories hyp))))))

(defn chosen-cats
  [workspace]
  (let [dochyps (group-by :docid (map #(lookup-hyp workspace %)
                                    (concat (:category (:accepted workspace))
                                            (:catpair-both (:accepted workspace))
                                            (:catpair-only-left (:accepted workspace))
                                            (:catpair-only-right (:accepted workspace)))))]
    (reduce (fn [m [docid hyps]]
         (let [prior-cats (get m docid)]
           (if prior-cats (update-in m [docid] set/union (set (mapcat :categories hyps)))
               (assoc m docid (set (mapcat :categories hyps))))))
       {} (seq dochyps))))

(defn evaluate
  [truedata est]
  (let [ep (cur-ep est)
        time-now (:time ep)
        doccats (chosen-cats (:workspace ep))
        [tp fp fn] (let [docid (first (get (:test truedata) (dec time-now)))]
                     (let [true-cats (get (:cats truedata) docid)
                           chosen-cats (get doccats docid #{})]
                       [(count (set/intersection true-cats chosen-cats))
                        (count (set/difference chosen-cats true-cats))
                        (count (set/difference true-cats chosen-cats))]))
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
