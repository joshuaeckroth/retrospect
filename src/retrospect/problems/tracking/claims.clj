(ns retrospect.problems.tracking.claims
  (:use [geppetto.claim])
  (:use [geppetto.stats]))

(def generic-claims
  [(make-claim tracking-abd-params
               (parameters {:control {:ContrastPreference "arbitrary"
                                      :HypPreference "arbitrary"}
                            :comparison {:ContrastPreference "delta,score"
                                         :HypPreference "score"}})
               (verify {:comparative ((> (geppetto.stats/mean :_DiffAvgPrec) 0.3)
                                      (> (geppetto.stats/mean :_DiffAvgCoverage) 0.3))}))])

(def noise-claims
  [(make-claim tracking-insertion-noise
               (parameters {:control {:Noise true
                                      :SensorInsertionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorInsertionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (> (first (:coefs lm)) 0.0)
                                         ;; strong correlation
                                         (> (:r-square lm) 0.5))))}))
   (make-claim tracking-distortion-noise
               (parameters {:control {:Noise true
                                      :SensorDistortionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDistortionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (> (first (:coefs lm)) 0.0)
                                         ;; strong correlation
                                         (> (:r-square lm) 0.5))))}))
   (make-claim tracking-duplication-noise
               (parameters {:control {:Noise true
                                      :SensorDuplicationNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDuplicationNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (> (first (:coefs lm)) 0.0)
                                         ;; strong correlation
                                         (> (:r-square lm) 0.5))))}))
   (make-claim tracking-deletion-noise
               (parameters {:control {:Noise true
                                      :SensorDeletionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDeletionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (> (first (:coefs lm)) 0.0)
                                         ;; strong correlation
                                         (> (:r-square lm) 0.5))))}))])

(def tracking-claims (concat generic-claims noise-claims))
