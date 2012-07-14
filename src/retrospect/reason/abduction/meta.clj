(ns retrospect.reason.abduction.meta
  (:require [clojure.set :as set])
  (:use [retrospect.epistemicstates :only
         [cur-ep new-child-ep new-branch-ep init-est ep-state-depth
          update-est nth-previous-ep print-est goto-ep
          get-init-workspace]])
  (:use [retrospect.reason.abduction.workspace :only
         [get-no-explainers new-hyp init-workspace calc-doubt
          explain add-kb add-observation add lookup-hyp reset-workspace
          update-kb explain update-hypotheses add-sensor-hyps]])
  (:use [retrospect.state]))

(defn reason
  [truedata workspace time-prev time-now sensors]
  (let [ws (if (= "none" (:Oracle params)) workspace
               (assoc workspace :oracle
                      (partial (:true-hyp?-fn (:abduction @problem))
                               truedata time-now)))]
    (if sensors
      (update-kb (explain (update-hypotheses
                           (add-sensor-hyps ws time-prev time-now sensors)
                           time-now)))
      (update-kb (explain (update-hypotheses ws time-now))))))

(defn metareasoning-activated?
  "Check if any of the metareasoning activation conditions are met."
  [est]
  (let [workspace (:workspace (cur-ep est))]
    (not-empty (get-no-explainers workspace))))

(defn workspace-better?
  [ws-new ws-old]
  ;; we wanted to fix "no explainers"; so, did we?
  (empty? (get-no-explainers ws-new)))

(defn belief-revision
  [noexp-hyp workspace]
  ;; reset workspace to ensure sensor hyps are all added new, and all
  ;; beliefs are gone
  (reset-workspace workspace))

(defn ignore-hyp
  [noexp-hyp workspace]
  (add-kb (reset-workspace workspace)
          [(new-hyp "Ignore" :kb :ignore 1.0 false
                    (:conflicts?-fn (:hyp noexp-hyp))
                    []
                    (format "Ignore %s" (:hyp noexp-hyp))
                    (format "Ignore %s" (:hyp noexp-hyp))
                    {:vertex (:vertex (:hyp noexp-hyp))})]))

(defn learn-hyp
  [noexp-hyp workspace]
  ;; ask problem domain to create new kb hyps that take into account
  ;; this noexp hyp is "true" (i.e., should be learned)
  workspace)

(defn apply-resolutions
  [accepted est time-prev time-now sensors]
  (let [new-est (new-branch-ep est (cur-ep est))
        new-ep (cur-ep new-est)
        ws-old (:workspace new-ep)
        ;; apply all the actions specified by the accepted meta-hyps
        ws-new (reduce (fn [ws h] ((:action h) ws)) ws-old accepted)
        ws-expl (reason (when (:Oracle params) truedata) ws-new
                        time-prev time-now sensors)
        new-expl-est (update-est new-est (assoc new-ep :workspace ws-expl))]
    (if (workspace-better? ws-expl ws-old)
      {:est new-expl-est
       :considered? true
       :accepted-branch? true}
      {:est (goto-ep new-expl-est (:id (cur-ep est)))
       :considered? true
       :accepted-branch? false})))

(defn make-noexp-hyps
  [noexp]
  (for [ne noexp]
    (new-hyp "NoExp" :noexp :noexp 1.0 true nil []
             (format "NoExp: %s" ne)
             (format "%s has no explainer but needs an explanation." ne)
             {:hyp ne})))

(defn conflicts?
  [hyp1 hyp2]
  (= (:explains hyp1) (:explains hyp2)))

(defn make-meta-hyps
  "Create explanations, and associated actions, for noexp."
  [ws-original noexp-hyps]
  (concat
   ;; anomaly hyps
   (map (fn [ne] (new-hyp "Anomaly" :anomaly :anomaly
                       0.0 ;(calc-doubt ws-original)
                       false conflicts? [(:contents ne)]
                       (format "%s is an anomaly" ne) (format "%s is an anomaly" ne)
                       {:action (partial belief-revision ne) :noexp-hyp ne}))
      noexp-hyps)
   ;; noise hyps
   (mapcat (fn [ne]
             [(new-hyp "Noise" :noise :insertion-noise
                       1.0 ;(+ 0.5 (* 0.5 (/ (double (:SensorInsertionNoise params)) 100.0)))
                       false conflicts? [(:contents ne)]
                       (format "%s is insertion noise" ne) (format "%s is insertion noise" ne)
                       {:action (partial ignore-hyp ne) :noexp-hyp ne})
              (new-hyp "Noise" :noise :distortion-noise
                       1.0 ;(+ 0.5 (* 0.5 (/ (double (:SensorDistortionNoise params)) 100.0)))
                       false conflicts? [(:contents ne)]
                       (format "%s is distortion noise" ne) (format "%s is distortion noise" ne)
                       {:action (partial ignore-hyp ne) :noexp-hyp ne})])
           (filter #(= :observation (:type (:hyp %))) noexp-hyps))
   ;; learn hyps
   (map (fn [ne] (new-hyp "Learn" :learn :learn
                       (- 1.0 (/ (double (:Knowledge params)) 100.0))
                       false conflicts? [(:contents ne)]
                       (format "%s should be learned" ne)
                       (format "%s should be learned" ne)
                       {:action (partial learn-hyp ne) :noexp-hyp ne}))
      noexp-hyps)))

(defn metareason
  [truedata est time-prev time-now sensors]
  (let [workspace (:workspace (cur-ep est))
        noexp (map (partial lookup-hyp workspace) (get-no-explainers workspace))
        noexp-hyps (make-noexp-hyps noexp)
        meta-hyps (make-meta-hyps workspace noexp-hyps)
        meta-ws (reduce add (reduce add-observation (init-workspace) noexp-hyps) meta-hyps)
        meta-ws-explained (explain meta-ws)
        ep-meta (assoc (cur-ep est) :meta-workspace meta-ws-explained)
        est-meta (update-est est ep-meta)
        accepted (map (partial lookup-hyp meta-ws-explained)
                    (concat (:anomaly (:accepted meta-ws-explained))
                            (:noise (:accepted meta-ws-explained))
                            (:learn (:accepted meta-ws-explained))))]
    (apply-resolutions accepted est-meta time-prev time-now sensors)))
