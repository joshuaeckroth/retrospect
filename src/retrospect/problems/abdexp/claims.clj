(ns retrospect.problems.abdexp.claims
  (:use [geppetto.claim])
  (:use [geppetto.stats]))

(def generic-claims
  [(make-claim tracking-baseline-high-avgprec
               (parameters "AbdExp/baseline")
               (verify {:control ((> (geppetto.stats/mean :_AvgPrec) 0.75)
                                  (> (geppetto.stats/mean :_AvgCoverage) 0.75))}))])

(def abduction-claims
  [])

