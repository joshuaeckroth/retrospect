(ns simulator.strategies.composite
  (:use [simulator.strategies.substrategies]))

(def strategy-funcs
  {"guess" [guess]
   "smartguess" [smartguess]
   "es-guess" [essentials guess]
   "es-smartguess" [essentials smartguess]
   "es-b1-guess" [essentials (best 1) guess]
   "es-sb1-guess" [essentials (smartbest 1) guess]
   "es-b1-smartguess" [essentials (best 1) smartguess]
   "es-sb1-smartguess" [essentials (smartbest 1) smartguess]
   "es-b2-b1-guess" [essentials (best 2) (best 1) guess]
   "es-sb2-sb1-guess" [essentials (smartbest 2) (smartbest 1) guess]
   "es-b2-b1-smartguess" [essentials (best 2) (best 1) smartguess]
   "es-sb2-sb1-smartguess" [essentials (smartbest 2) (smartbest 1) smartguess]
   "es-b3-b2-b1-guess" [essentials (best 3) (best 2) (best 1) guess]
   "es-sb3-sb2-sb1-guess" [essentials (smartbest 3) (smartbest 2) (smartbest 1) guess]
   "es-b3-b2-b1-smartguess" [essentials (best 3) (best 2) (best 1) smartguess]
   "es-sb3-sb2-sb1-smartguess" [essentials (smartbest 3) (smartbest 2)
                                (smartbest 1) smartguess]
   "es-b4-b3-b2-b1-smartguess" [essentials (best 4) (best 3) (best 2) (best 1) smartguess]
   "es-sb4-sb3-sb2-sb1-smartguess" [essentials (smartbest 4) (smartbest 3) (smartbest 2)
                                (smartbest 1) smartguess]
   "es" [essentials]
   "es-b1" [essentials (best 1)]
   "es-sb1" [essentials (smartbest 1)]
   "es-b2-b1" [essentials (best 2) (best 1)]
   "es-sb2-sb1" [essentials (smartbest 2) (smartbest 1)]
   "es-b3-b2-b1" [essentials (best 3) (best 2) (best 1)]
   "es-sb3-sb2-sb1" [essentials (smartbest 3) (smartbest 2) (smartbest 1)]
   "es-sb4-sb3-sb2-sb1" [essentials (smartbest 4) (smartbest 3)
                         (smartbest 2) (smartbest 1)]})

(def strategies (sort (keys strategy-funcs)))

;;(def strategies ["es-sb4-sb3-sb2-sb1-smartguess"])


