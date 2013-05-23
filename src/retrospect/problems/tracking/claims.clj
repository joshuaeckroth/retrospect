(ns retrospect.problems.tracking.claims
  (:use [geppetto.claim])
  (:use [geppetto.stats]))

(def generic-claims
  [(make-claim tracking-baseline
               (parameters {:control {:dummy 0}}) ;; use only default params
               (verify {:control ((clojure.test/is (> (geppetto.stats/mean :_AvgPrec) 0.8))
                                  (clojure.test/is (> (geppetto.stats/mean :_AvgCoverage) 0.8))
                                  (clojure.test/is (> (geppetto.stats/mean :_AvgF1) 0.8))
                                  (let [lm (geppetto.stats/linear-reg :_Doubt :_Prec)]
                                    ;; negative slope
                                    (and (clojure.test/is (< (first (:coefs lm)) 0.0))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.3)))))}))
   (make-claim tracking-abd-params
               (parameters {:control {:ContrastPreference "arbitrary"
                                      :HypPreference "arbitrary"}
                            :comparison {:ContrastPreference "delta,score"
                                         :HypPreference "score"}})
               (verify {:comparative ((clojure.test/is (> (geppetto.stats/mean :_DiffAvgPrec) 0.3))
                                      (clojure.test/is (> (geppetto.stats/mean :_DiffAvgCoverage) 0.3))
                                      (clojure.test/is (> (geppetto.stats/mean :_DiffAvgF1) 0.3)))}))])

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
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.5))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6)))))}))
   (make-claim tracking-distortion-noise
               ;; we don't get a good correlation with noexppct with this kind of noise
               (parameters {:control {:Noise true
                                      :SensorDistortionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDistortionNoise :_ErrorsNoise)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.35))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorDistortionNoise :_AvgPrec)]
                                    ;; negative slope
                                    (and (clojure.test/is (< (first (:coefs lm)) 0.0))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.3)))))}))
   (make-claim tracking-duplication-noise
               (parameters {:control {:Noise true
                                      :SensorDuplicationNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDuplicationNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.0))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorDuplicationNoise :_NoExpReasonConflict)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.14))
                                         ;; strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.6)))))}))
   (make-claim tracking-deletion-noise
               (parameters {:control {:Noise true
                                      :SensorDeletionNoise [0 5 10 15 20 25 30 35 40 45 50]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_SensorDeletionNoise :_NoExplainersPct)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.0))
                                         ;; weak correlation
                                         (clojure.test/is (> (:r-square lm) 0.4))))
                                  (let [lm (geppetto.stats/linear-reg :_SensorDeletionNoise :_Coverage)]
                                    ;; negative slope
                                    (and (clojure.test/is (< (first (:coefs lm)) 0.0))
                                         ;; moderate correlation
                                         (clojure.test/is (> (:r-square lm) 0.5)))))}))])

(def minscore-claims
  [(make-claim tracking-minscore
               (parameters {:control {:MinScore [0 10 20 30 40 50 60 70 80 90 100]}})
               (verify {:control ((let [lm (geppetto.stats/linear-reg :_MinScore :_ErrorsMinScore)]
                                    ;; positive slope
                                    (and (clojure.test/is (> (first (:coefs lm)) 0.3))
                                         ;; very strong correlation
                                         (clojure.test/is (> (:r-square lm) 0.8)))))}))])

(def tracking-claims (concat generic-claims noise-claims minscore-claims))
