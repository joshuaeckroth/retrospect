(ns simulator.strategies.essentials-guess
  (:use clojure.set))

(defn explain-essentials-guess
  [strat-state]
  (let [conflicts (find-conflicts (:hypspace strat-state) (:accepted strat-state))
	unexplained (find-unexplained (:hypspace strat-state) (:accepted strat-state))
	essentials (find-essentials (:hypspace strat-state) unexplained)]
    (case
     
     ;; remove any conflicts
     (not-empty conflicts)
     (recur (-> strat-state
		(update-in [:rejected] union conflicts)
		(update-in [:considering] difference conflicts)))

     ;; accept essentials
     (not-empty essentials)
     (recur (-> strat-state
		(update-in [:accepted] union essentials)
		(update-in [:considering] difference essentials)))

     ;; no more essentials, so refer to explain-guess for the rest
     (explain-guess strat-state))))