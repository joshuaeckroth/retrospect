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
    (not-empty (ws/find-no-explainers (:log workspace)))))

(comment (let [comp-cov (- (compare (:coverage ws1) (:coverage ws2)))]
           (if (not= 0 comp-cov) comp-cov
               (compare (:doubt ws1) (:doubt ws2)))))

(defn workspace-compare
  [ws-new ws-old]
  (if (< (ws/calc-doubt ws-new) (ws/calc-doubt ws-old)) -1 1))
