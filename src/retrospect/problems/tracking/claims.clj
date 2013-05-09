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


