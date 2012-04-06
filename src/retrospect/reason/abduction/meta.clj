(ns retrospect.reason.abduction.meta
  (:require [retrospect.reason.abduction.workspace :as ws])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [est]
  (let [workspace (:workspace (cur-ep est))]
      ;; TODO: implement other conditions
      (or (not-empty (:no-explainers (:log workspace)))
          (> 0.90 (:coverage workspace))
          (< 0.10 (:doubt workspace)))))

(defn workspace-compare
  [ws1 ws2]
  (let [comp-cov (- (compare (:coverage ws1) (:coverage ws2)))]
    (if (not= 0 comp-cov) comp-cov
        (compare (:doubt ws1) (:doubt ws2)))))
