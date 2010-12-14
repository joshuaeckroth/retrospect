(ns simulator.strategies.composite
  (:import [java.awt Color])
  (:use [simulator.strategies.substrategies]))

(def strategy-info
  {"guess" {:color Color/red :funcs [guess]}
   "smartguess" {:color Color/red :funcs [smartguess]}
   "es-guess" {:color Color/blue :funcs [essentials guess]}
   "es-smartguess" {:color Color/blue :funcs [essentials smartguess]}
   "es-b1-guess" {:color Color/green :funcs [essentials (best 1) guess]}
   "es-sb1-guess" {:color Color/green :funcs [essentials (smartbest 1) guess]}
   "es-b1-smartguess" {:color Color/green :funcs [essentials (best 1) smartguess]}
   "es-sb1-smartguess" {:color Color/green :funcs [essentials (smartbest 1) smartguess]}
   "es-b2-b1-guess" {:color Color/orange :funcs [essentials (best 2) (best 1) guess]}
   "es-sb2-sb1-guess" {:color Color/orange
                       :funcs [essentials (smartbest 2) (smartbest 1) guess]}
   "es-b2-b1-smartguess" {:color Color/orange
                          :funcs [essentials (best 2) (best 1) smartguess]}
   "es-sb2-sb1-smartguess" {:color Color/orange
                            :funcs [essentials (smartbest 2) (smartbest 1) smartguess]}
   "es-b3-b2-b1-guess" {:color Color/pink
                        :funcs [essentials (best 3) (best 2) (best 1) guess]}
   "es-sb3-sb2-sb1-guess" {:color Color/pink
                           :funcs [essentials (smartbest 3) (smartbest 2)
                                   (smartbest 1) guess]}
   "es-b3-b2-b1-smartguess" {:color Color/pink
                             :funcs [essentials (best 3) (best 2) (best 1) smartguess]}
   "es-sb3-sb2-sb1-smartguess" {:color Color/pink
                                :funcs [essentials (smartbest 3) (smartbest 2)
                                        (smartbest 1) smartguess]}
   "es-b4-b3-b2-b1-smartguess" {:color Color/magenta
                                :funcs [essentials (best 4) (best 3) (best 2)
                                        (best 1) smartguess]}
   "es-sb4-sb3-sb2-sb1-smartguess" {:color Color/magenta
                                    :funcs [essentials (smartbest 4) (smartbest 3)
                                            (smartbest 2) (smartbest 1) smartguess]}
   "es" {:color Color/lightGray :funcs [essentials]}
   "es-b1" {:color Color/cyan :funcs [essentials (best 1)]}
   "es-sb1" {:color Color/cyan :funcs [essentials (smartbest 1)]}
   "es-b2-b1" {:color Color/black :funcs [essentials (best 2) (best 1)]}
   "es-sb2-sb1" {:color Color/black :funcs [essentials (smartbest 2) (smartbest 1)]}
   "es-b3-b2-b1" {:color Color/darkGray :funcs [essentials (best 3) (best 2) (best 1)]}
   "es-sb3-sb2-sb1" {:color Color/darkGray
                     :funcs [essentials (smartbest 3) (smartbest 2) (smartbest 1)]}
   "es-sb4-sb3-sb2-sb1" {:color Color/yellow
                         :funcs [essentials (smartbest 4) (smartbest 3)
                                 (smartbest 2) (smartbest 1)]}})

;;(def strategies (sort (keys strategy-funcs)))

(def strategies ["guess" "es" "es-sb2-sb1"])


