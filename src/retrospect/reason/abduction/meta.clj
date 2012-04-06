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
          (< 0.10 (ws/get-unexp-pct workspace))
          (< 0.10 (:doubt workspace)))))

(defn workspace-compare
  [ws1 ws2]
  (let [comp-unexp (compare (ws/get-unexp-pct ws1)
                            (ws/get-unexp-pct ws2))]
    (if (not= 0 comp-unexp) comp-unexp
        (compare (:doubt ws1) (:doubt ws2)))))
