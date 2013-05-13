(ns retrospect.problems.abdexp.claims
  (:use [geppetto.claim])
  (:use [geppetto.stats]))

(def generic-claims
  [(make-claim abdexp-baseline
               (parameters {:control {}}) ;; use only default params
               (verify {:control ((clojure.test/is (> (geppetto.stats/mean :_AvgMPEPrec) 0.95))
                                  (clojure.test/is (> (geppetto.stats/mean :_AvgMPECoverage) 0.48))
                                  (clojure.test/is (> (geppetto.stats/mean :_AvgMPEF1) 0.61))
                                  (let [lm (geppetto.stats/linear-reg :_Doubt :_MPEPrec)]
                                    ;; negative slope
                                    (and (clojure.test/is (< (first (:coefs lm)) 0.0))
                                         ;; moderate correlation
                                         (clojure.test/is (> (:r-square lm) 0.4)))))}))
   (make-claim abdexp-abd-params
               (parameters {:control {:ContrastPreference "arbitrary"
                                      :HypPreference "arbitrary"}
                            :comparison {:ContrastPreference "delta,score"
                                         :HypPreference "score,expl"}})
               (verify {:comparative ((clojure.test/is (> (geppetto.stats/mean :_DiffAvgMPEPrec) 0.15))
                                      (clojure.test/is (> (geppetto.stats/mean :_DiffAvgMPECoverage) 0.07))
                                      (clojure.test/is (> (geppetto.stats/mean :_DiffAvgMPEF1) 0.10)))}))])

(def noise-claims
  [(make-claim abdexp-insertion-noise
               (parameters {:control {:Noise true
                                      :SensorInsertionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorInsertionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.001))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.25))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorInsertionNoise :_ErrorsNoise)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.2))
                                         ;; moderate correlation
                                         (clojure.test/is (> (:r-square lm) 0.4)))))}))
   (make-claim abdexp-distortion-noise
               ;; we don't get a good correlation with noexppct with this kind of noise
               (parameters {:control {:Noise true
                                      :SensorDistortionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDistortionNoise :_ErrorsNoise)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.3))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorDistortionNoise :_AvgMPEPrec)]
                                    ;; negative slope
                                    (and (clojure.test/is (< (first (:coefs lm)) 0.0))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.25)))))}))
   (make-claim abdexp-duplication-noise
               (parameters {:control {:Noise true
                                      :SensorDuplicationNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDuplicationNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.0))
                                         ;; moderate correlation
                                         (clojure.test/is (> (:r-square lm) 0.45))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorDuplicationNoise :_NoExpReasonConflict)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.15))
                                         ;; moderate correlation
                                         (clojure.test/is (> (:r-square lm) 0.5)))))}))
   (make-claim abdexp-deletion-noise
               (parameters {:control {:Noise true
                                      :SensorDeletionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDeletionNoise :_MPECoverage)]
                                    ;; negative slope
                                    (and (clojure.test/is (< (first (:coefs lm)) 0.0))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.25)))))}))])

(def minscore-claims
  [(make-claim abdexp-minscore
               (parameters {:control {:MinScore [0 10 20 30 40 50 60 70 80 90 100]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_MinScore :_ErrorsMinScore)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.3))
                                         ;; very strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.8)))))}))])

(def abduction-claims (concat generic-claims noise-claims minscore-claims))

