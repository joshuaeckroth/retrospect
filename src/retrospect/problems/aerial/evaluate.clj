(ns retrospect.problems.aerial.evaluate
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [retrospect.evaluate])
  (:use [retrospect.epistemicstates :only [cur-ep goto-cycle decision-points]])
  (:use [retrospect.reason.abduction.workspace :only [accepted rejected hypotheses]])
  (:use [retrospect.state]))

(defn near?
  [x1 x2]
  (< (Math/abs (- x1 x2)) 1.0))

(defn det-obj-near?
  [det obj]  
  (and (near? (:x det) (:x obj))
       (near? (:y det) (:y obj))))

(defn true-hyp?
  [truedata hyp]
  (let [frames (:truth truedata)]
    (cond (= :observation (:type hyp))
          (let [det (:det hyp)]
            (not= nil (:objid (first (filter #(det-obj-near? det %) (:objects (get frames (:time det))))))))
          (= :movement (:type hyp))
          (let [{:keys [det det2]} hyp
                objid1 (:objid (first (filter #(det-obj-near? det %) (:objects (get frames (:time det))))))
                objid2 (:objid (first (filter #(det-obj-near? det2 %) (:objects (get frames (:time det2))))))]
            (and (not= nil objid1) (= objid1 objid2))))))

(defn moves-match?
  [mov1 mov2]
  (and (near? (:x mov1) (:x mov2))
       (near? (:ox mov1) (:ox mov2))
       (near? (:y mov1) (:y mov2))
       (near? (:oy mov1) (:oy mov2))
       (= (:time mov1) (:time mov2))
       (= (:ot mov1) (:ot mov2))))

(defn count-matches
  [true-movs movs]
  (count (filter (fn [m] (some #(moves-match? m %) true-movs)) movs)))

(defn tp-tn-fp-fn
  [true-movs acc-movs not-acc-movs]
  (if (empty? true-movs) [0 0 0 0]
      (let [true-pos (count-matches true-movs acc-movs)
            false-pos (- (count acc-movs) true-pos)
            false-neg (count-matches true-movs not-acc-movs)
            true-neg (- (count not-acc-movs) false-neg)]
        [true-pos true-neg false-pos false-neg])))

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

(defn get-true-movements
  [truedata time-now]
  (set (filter #(and (:ot %) (<= (:time %) time-now))
               (:all-moves truedata))))

(defn evaluate
  [truedata est]
  (if (or (and (not training?) (not @batch))
          (and (not training?) (= (:Steps params) (:time (cur-ep est)))))
    (let [metrics
          (for [ep (decision-points est)]
            (let [ws (:workspace ep)
                  time-now (:time ep)
                  true-moves (get-true-movements truedata time-now)
                  acc-movs (map :mov (:movement (accepted ws)))
                  rej-movs (map :mov (:movement (rejected ws)))
                  [tp tn fp fn] (tp-tn-fp-fn true-moves acc-movs rej-movs)
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
