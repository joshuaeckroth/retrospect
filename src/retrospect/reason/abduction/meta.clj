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
    (comment (or (not-empty (:no-explainers (:log workspace)))
                 (> 0.90 (:coverage workspace))
                 (< 0.10 (:doubt workspace))))
    true))

(comment (let [comp-cov (- (compare (:coverage ws1) (:coverage ws2)))]
           (if (not= 0 comp-cov) comp-cov
               (compare (:doubt ws1) (:doubt ws2)))))

(defn workspace-compare
  [ws-new ws-old]
  (comment (if (< (:doubt ws-new) (* 1.5 (:doubt ws-old))) -1 1))
  -1)
