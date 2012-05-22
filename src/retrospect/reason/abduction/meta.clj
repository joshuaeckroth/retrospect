(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [est]
  (let [workspace (:workspace (cur-ep est))]
    ;; TODO: implement other conditions
    (comment (or (not-empty (ws/find-no-explainers (:log workspace)))
                 (> 0.90 (ws/calc-coverage workspace))
                 (< 0.10 (ws/calc-doubt workspace))))
    true))

(comment (let [comp-cov (- (compare (:coverage ws1) (:coverage ws2)))]
           (if (not= 0 comp-cov) comp-cov
               (compare (:doubt ws1) (:doubt ws2)))))

(defn workspace-compare
  [ws-new ws-old]
  (comment (if (< (ws/calc-doubt ws-new) (* 1.5 (ws/calc-doubt ws-old))) -1 1))
  -1)

(def prob-yes-corr (agent 0))
(def prob-yes-incorr (agent 0))
(def prob-no-corr (agent 0))
(def prob-no-incorr (agent 0))
(def conf-yes-corr (agent 0))
(def conf-yes-incorr (agent 0))
(def conf-no-corr (agent 0))
(def conf-no-incorr (agent 0))


(defn meta-resolve
  [ws-new ws-old meta-counts meta-truedata time in-meta-training?]
  (println meta-counts)
  (let [acc-old (set (apply concat (vals (:accepted ws-old))))
        acc-new (set (apply concat (vals (:accepted ws-new))))
        acc-diff (set/difference acc-new acc-old)
        conflict-pairs (mapcat (fn [h]
                                 (map (fn [c] [h c])
                                      (ws/find-conflicts-selected ws-old h acc-old)))
                               acc-diff)
        non-conflicting (reduce (fn [hs h] (disj hs h)) acc-diff (map first conflict-pairs))
        meta-counts-updated (if-not in-meta-training?
                              meta-counts
                              (reduce
                               (fn [m [h c]]
                                 (let [h-pref-count (get-in m [(:type h) (:type c)] 0)
                                       c-pref-count (get-in m [(:type c) (:type h)] 0)]
                                   (if ((:true-hyp?-fn (:abduction @problem))
                                        meta-truedata time h)
                                     (assoc-in m [(:type h) (:type c)]
                                               (inc h-pref-count))
                                     (assoc-in m [(:type c) (:type h)]
                                               (inc c-pref-count)))))
                               meta-counts conflict-pairs))
        ws (reduce (fn [ws h] (ws/revise ws h)) ws-old non-conflicting)
        ws-resolved (if-not in-meta-training?
                      (reduce
                       (fn [ws [h c]]
                         (let [h-pref-count (get-in meta-counts [(:type h) (:type c)] 0)
                               c-pref-count (get-in meta-counts [(:type c) (:type h)] 0)
                               h-prob (if (= 0 (+ h-pref-count c-pref-count)) 0.0
                                          (/ (double h-pref-count)
                                             (double (+ h-pref-count c-pref-count))))]
                           (println (format (str "h = %s %s conf: %.2f ; "
                                                 "c = %s %s conf: %.2f ; "
                                                 "h-prob: %.2f, c-prob: %.2f ; "
                                                 "choose h over c? %s ; this correct? %s")
                                            h ((:true-hyp?-fn (:abduction @problem))
                                               meta-truedata time h)
                                            (ws/hyp-conf ws-new h)
                                            c ((:true-hyp?-fn (:abduction @problem))
                                               meta-truedata time c)
                                            (ws/hyp-conf ws-old c)
                                            (* h-prob (ws/hyp-conf ws-new h))
                                            (* (- 1.0 h-prob) (ws/hyp-conf ws-old c))
                                            (> (* h-prob (ws/hyp-conf ws-new h))
                                               (* (- 1.0 h-prob) (ws/hyp-conf ws-old c)))
                                            (or (and (> (* h-prob (ws/hyp-conf ws-new h))
                                                        (* (- 1.0 h-prob) (ws/hyp-conf ws-old c)))
                                                     ((:true-hyp?-fn (:abduction @problem))
                                                      @truedata time h))
                                                (and (not (> (* h-prob (ws/hyp-conf ws-new h))
                                                             (* (- 1.0 h-prob) (ws/hyp-conf ws-old c))))
                                                     (not ((:true-hyp?-fn (:abduction @problem))
                                                           @truedata time h))))))
                           (when (and (> (* h-prob (ws/hyp-conf ws-new h))
                                         (* (- 1.0 h-prob) (ws/hyp-conf ws-old c)))
                                      ((:true-hyp?-fn (:abduction @problem))
                                       @truedata time h))
                             (send prob-yes-corr inc))
                           (when (and (> (* h-prob (ws/hyp-conf ws-new h))
                                         (* (- 1.0 h-prob) (ws/hyp-conf ws-old c)))
                                      (not ((:true-hyp?-fn (:abduction @problem))
                                            @truedata time h)))
                             (send prob-yes-incorr inc))
                           (when (and (not (> (* h-prob (ws/hyp-conf ws-new h))
                                              (* (- 1.0 h-prob) (ws/hyp-conf ws-old c))))
                                      (not ((:true-hyp?-fn (:abduction @problem))
                                            @truedata time h)))
                             (send prob-no-corr inc))
                           (when (and (not (> (* h-prob (ws/hyp-conf ws-new h))
                                              (* (- 1.0 h-prob) (ws/hyp-conf ws-old c))))
                                      ((:true-hyp?-fn (:abduction @problem))
                                       @truedata time h))
                             (send prob-no-incorr inc))
                           (when (and (> (ws/hyp-conf ws-new h)
                                         (ws/hyp-conf ws-old c))
                                      ((:true-hyp?-fn (:abduction @problem))
                                       @truedata time h))
                             (send conf-yes-corr inc))
                           (when (and (> (ws/hyp-conf ws-new h)
                                         (ws/hyp-conf ws-old c))
                                      (not ((:true-hyp?-fn (:abduction @problem))
                                            @truedata time h)))
                             (send conf-yes-incorr inc))
                           (when (and (not (> (ws/hyp-conf ws-new h)
                                              (ws/hyp-conf ws-old c)))
                                      (not ((:true-hyp?-fn (:abduction @problem))
                                            @truedata time h)))
                             (send conf-no-corr inc))
                           (when (and (not (> (ws/hyp-conf ws-new h)
                                              (ws/hyp-conf ws-old c)))
                                      ((:true-hyp?-fn (:abduction @problem))
                                       @truedata time h))
                             (send conf-no-incorr inc))
                           (comment ((:true-hyp?-fn (:abduction @problem))
                                     @truedata time h))
                           (if (> (* h-prob (ws/hyp-conf ws-new h))
                                  (* (- 1.0 h-prob) (ws/hyp-conf ws-old c)))
                             (ws/revise ws h) ws)))
                       ws conflict-pairs)
                      (reduce (fn [ws h] (ws/revise ws h))
                              ws
                              (filter #((:true-hyp?-fn (:abduction @problem))
                                        meta-truedata time %)
                                      (set/union acc-old acc-new))))]
    (println "prob-yes-corr" @prob-yes-corr "prob-no-corr" @prob-no-corr "prob-yes-incorr" @prob-yes-incorr "prob-no-incorr" @prob-no-incorr "conf-yes-corr" @conf-yes-corr "conf-no-corr" @conf-no-corr "conf-yes-incorr" @conf-yes-incorr "conf-no-incorr" @conf-no-incorr)
    [meta-counts-updated ws-resolved]))
