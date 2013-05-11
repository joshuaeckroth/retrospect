(ns retrospect.problems.tracking.claims
  (:use [geppetto.claim])
  (:use [geppetto.stats]))

(def generic-claims
  [(make-claim tracking-abd-params
               (parameters {:control {:ContrastPreference "arbitrary"
                                      :HypPreference "arbitrary"}
                            :comparison {:ContrastPreference "delta,score"
                                         :HypPreference "score"}})
               (verify {:comparative ((clojure.test/is (> (geppetto.stats/mean :_DiffAvgPrec) 0.3))
                                      (clojure.test/is (> (geppetto.stats/mean :_DiffAvgCoverage) 0.3)))}))])

(def noise-claims
  [(make-claim tracking-insertion-noise
               (parameters {:control {:Noise true
                                      :SensorInsertionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorInsertionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.001))
                                         ;; moderate correlation
                                         (clojure.test/is (> (:r-square lm) 0.5))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorInsertionNoise :_ErrorsNoise)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.4))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6)))))}))
   (make-claim tracking-distortion-noise
               ;; we don't get a good correlation with noexppct with this kind of noise
               (parameters {:control {:Noise true
                                      :SensorDistortionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDistortionNoise :_ErrorsNoise)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.2))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6)))))}))
   (make-claim tracking-duplication-noise
               (parameters {:control {:Noise true
                                      :SensorDuplicationNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDuplicationNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.0))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6)))))}))
   (make-claim tracking-deletion-noise
               (parameters {:control {:Noise true
                                      :SensorDeletionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDeletionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.0))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.4)))))}))])

(def tracking-claims (concat generic-claims noise-claims))
