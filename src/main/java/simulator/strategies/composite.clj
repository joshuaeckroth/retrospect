(ns simulator.strategies.composite
  (:import [java.awt Color])
  (:use [simulator.strategies.substrategies]))

(def strategy-info
  {"guess" {:color Color/red :funcs [guess]}
   "smartguess" {:color Color/red :funcs [smartguess]}
   "es-g" {:color Color/blue :funcs [essentials guess]}
   "es-sg" {:color Color/blue :funcs [essentials smartguess]}
   "es-b1-g" {:color Color/green :funcs [essentials (best 1) guess]}
   "es-sb1-g" {:color Color/green :funcs [essentials (smartbest 1) guess]}
   "es-b1-sg" {:color Color/green :funcs [essentials (best 1) smartguess]}
   "es-sb1-sg" {:color Color/green :funcs [essentials (smartbest 1) smartguess]}
   "es-b21-g" {:color Color/orange :funcs [essentials (best 2) (best 1) guess]}
   "es-sb21-g" {:color Color/orange
                :funcs [essentials (smartbest 2) (smartbest 1) guess]}
   "es-b21-sg" {:color Color/orange
                :funcs [essentials (best 2) (best 1) smartguess]}
   "es-sb21-sg" {:color Color/orange
                 :funcs [essentials (smartbest 2) (smartbest 1) smartguess]}
   "es-b321-g" {:color Color/pink
                :funcs [essentials (best 3) (best 2) (best 1) guess]}
   "es-sb321-g" {:color Color/pink
                 :funcs [essentials (smartbest 3) (smartbest 2)
                         (smartbest 1) guess]}
   "es-b321-sg" {:color Color/pink
                 :funcs [essentials (best 3) (best 2) (best 1) smartguess]}
   "es-sb321-sg" {:color Color/pink
                  :funcs [essentials (smartbest 3) (smartbest 2)
                          (smartbest 1) smartguess]}
   "es-b4321-sg" {:color Color/magenta
                  :funcs [essentials (best 4) (best 3) (best 2)
                          (best 1) smartguess]}
   "es-sb4321-sg" {:color Color/magenta
                   :funcs [essentials (smartbest 4) (smartbest 3)
                           (smartbest 2) (smartbest 1) smartguess]}
   "es" {:color Color/lightGray :funcs [essentials]}
   "es-b1" {:color Color/cyan :funcs [essentials (best 1)]}
   "es-sb1" {:color Color/cyan :funcs [essentials (smartbest 1)]}
   "es-b21" {:color Color/black :funcs [essentials (best 2) (best 1)]}
   "es-sb21" {:color Color/black :funcs [essentials (smartbest 2) (smartbest 1)]}
   "es-b321" {:color Color/darkGray :funcs [essentials (best 3) (best 2) (best 1)]}
   "es-sb321" {:color Color/darkGray
               :funcs [essentials (smartbest 3) (smartbest 2) (smartbest 1)]}
   "es-sb4321" {:color Color/yellow
                :funcs [essentials (smartbest 4) (smartbest 3)
                        (smartbest 2) (smartbest 1)]}})

;;(def strategies (sort (keys strategy-funcs)))

(def strategies ["guess" "es" "es-sg" "es-sb4321" "es-sb4321-sg"])


