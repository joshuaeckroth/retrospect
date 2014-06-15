(ns retrospect.problems.tracking.export
  (:use [retrospect.problems.tracking.colors :only [color-str gray]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.state :only [params]]))

(defn export-abra
  [truedata sensors]
  (println "(make-exp-case")
  (println (format ";; Seed: %d" (:Seed params)))
  (println ":groundtruth '(")
  (doseq [mov (:all-moves truedata)]
    (println (format "(mov %d %d %d %d %d %d \"%s\")"
                     (:ox mov) (:oy mov)
                     (:x mov) (:y mov) (:ot mov) (:time mov)
                     (color-str (:color mov)))))
  (println ")")
  (println ":evidence '(")
  (doseq [t (range (inc (:Steps params)))]
    (println (format "(next-time-step %d %d)" t (inc t)))
    (doseq [s sensors]
      (doseq [det (sensed-at s t)]
        (when (not= gray (:color det))
          (println (format "(obj-color %d %d %d \"%s\")"
                           (:x det) (:y det) (:time det)
                           (color-str (:color det)))))
        ;; ignored: (:apriori det)
        (when (not= (dec (:Steps params)) t)
          (println (format "(det-a %d %d %d)"
                           (:x det) (:y det) (:time det))))
        (when (not= 0 t)
          (println (format "(det-b %d %d %d)"
                           (:x det) (:y det) (:time det)))))))
  ;; evidence
  (println ")")
  ;; make-exp-case
  (println ")"))

(defn export
  [export-type truedata sensors]
  (case export-type
    "abra" (export-abra truedata sensors)
    nil))

(defn export-header
  [export-type]
  (case export-type "abra" (println "(setq *cases* (list")
        nil))

(defn export-footer
  [export-type]
  (case export-type "abra" (println "))")))
