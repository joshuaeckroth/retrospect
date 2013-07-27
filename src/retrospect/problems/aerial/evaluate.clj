(ns retrospect.problems.aerial.evaluate
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate])
  (:use [retrospect.epistemicstates :only [cur-ep goto-cycle decision-points]])
  (:use [retrospect.reason.abduction.workspace :only [accepted rejected hypotheses]])
  (:use [retrospect.state]))

(defn near?
  [det obj]  
  (and (< (Math/abs (- (:x det) (:x obj))) 1.0)
       (< (Math/abs (- (:y det) (:y obj))) 1.0)))

(defn true-hyp?
  [truedata hyp]
  (let [frames (:truth truedata)]
    (cond (= :observation (:type hyp))
          (let [det (:det hyp)]
            (not= nil (:objid (first (filter #(near? det %) (:objects (get frames (:time det))))))))
          (= :movement (:type hyp))
          (let [{:keys [det det2]} hyp
                objid1 (:objid (first (filter #(near? det %) (:objects (get frames (:time det))))))
                objid2 (:objid (first (filter #(near? det2 %) (:objects (get frames (:time det2))))))]
            (and (not= nil objid1) (= objid1 objid2))))))

(defn tp-tn-fp-fn
  [truedata acc-mov-hyps not-acc-mov-hyps]
  (let [true-pos (count (filter #(true-hyp? truedata %) acc-mov-hyps))
        false-pos (- (count acc-mov-hyps) true-pos)
        false-neg (count (filter #(true-hyp? truedata %) not-acc-mov-hyps))
        true-neg (- (count not-acc-mov-hyps) false-neg)]
    [true-pos true-neg false-pos false-neg]))

(defn calc-prec-recall
  [tp tn fp fn]
  (let [recall (/ (double tp) (double (+ tp fn)))
        prec (/ (double tp) (double (+ tp fp)))]
    ;; http://en.wikipedia.org/wiki/Receiver_operating_characteristic
    {:TP tp :TN tn :FP fp :FN fn
     :TPR (/ (double tp) (double (+ tp fn)))
     :FPR (/ (double fp) (double (+ fp tn)))
     :Recall recall
     :Prec prec
     :F1 (/ (* 2.0 prec recall) (+ prec recall))}))

(defn evaluate
  [truedata est]
  (if (or (and (not training?) (not @batch))
          (and (not training?) (= (:Steps params) (:time (cur-ep est)))))
    (let [metrics
          (for [ep (decision-points est)]
            (let [ws (:workspace ep)
                  time-now (:time ep)
                  acc-movs (:movement (accepted ws))
                  not-acc-movs (set/difference (set (:movement (hypotheses ws))) (set acc-movs))
                  [tp tn fp fn] (tp-tn-fp-fn truedata acc-movs not-acc-movs)
                  true-det-scores (map (comp :detscore :det)
                                       (filter #(true-hyp? truedata %) (:observation (hypotheses ws))))
                  false-det-scores (map (comp :detscore :det)
                                        (filter #(not (true-hyp? truedata %)) (:observation (hypotheses ws))))
                  true-move-dists (map (comp :dist :mov)
                                       (filter #(true-hyp? truedata %) (:movement (hypotheses ws))))
                  false-move-dists (map (comp :dist :mov)
                                        (filter #(not (true-hyp? truedata %)) (:movement (hypotheses ws))))
                  true-move-avgpixels (map #(Math/abs (- (:avgpixel (:det %))
                                                         (:avgpixel (:det2 %))))
                                           (filter #(true-hyp? truedata %) (:movement (hypotheses ws))))
                  false-move-avgpixels (map #(Math/abs (- (:avgpixel (:det %))
                                                          (:avgpixel (:det2 %))))
                                            (filter #(not (true-hyp? truedata %)) (:movement (hypotheses ws))))]
              #_(spit "detscores.csv" (format "tf,score\n%s\n%s\n"
                                              (str/join "\n" (map #(format "TRUE,%f" %)
                                                                  true-det-scores))
                                              (str/join "\n" (map #(format "FALSE,%f" %)
                                                                  false-det-scores))))
              #_(spit "movedists.csv" (format "tf,dist,diffavgpixel\n%s\n%s\n"
                                              (str/join "\n" (map #(format "TRUE,%f,%f" %1 %2)
                                                                  true-move-dists true-move-avgpixels))
                                              (str/join "\n" (map #(format "FALSE,%f,%f" %1 %2)
                                                                  false-move-dists false-move-avgpixels))))
              (merge
               (calc-prec-recall tp tn fp fn)
               {:AvgTrueMovDist (avg true-move-dists)
                :AvgFalseMovDist (avg false-move-dists)
                :AvgTrueDetScore (avg true-det-scores)
                :AvgFalseDetScore (avg false-det-scores)})))]
      (merge (last metrics)
             {:AvgPrec (avg (map :Prec metrics))
              :AvgRecall (avg (map :Recall metrics))
              :AvgF1 (avg (map :F1 metrics))
              :AvgTPR (avg (map :TPR metrics))
              :AvgFPR (avg (map :FPR metrics))}))
    {:TP 0 :TN 0 :FP 0 :FN 0 :TPR 0.0 :FPR 0.0
     :F1 0.0 :Recall 0.0 :Prec 0.0
     :AvgTPR 0.0 :AvgFPR 0.0
     :AvgF1 0.0 :AvgPrec 0.0 :AvgRecall 0.0}))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                  [:TP :TN :FP :FN :TPR :FPR :F1 :Recall :Prec
                   :AvgTPR :AvgFPR
                   :AvgPrec :AvgRecall :AvgF1])))
