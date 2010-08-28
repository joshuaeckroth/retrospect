(ns simulator.strategies
  (:use simulator.types.hypotheses)
  (:use clojure.set))

(defn explain-guess
  [strat-state]
  (let [conflicts (find-conflicts (:hypspace strat-state) (:accepted strat-state))
	unexplained (find-unexplained (:hypspace strat-state) (:accepted strat-state))]
    (case
     
     ;; remove any conflicts
     (not-empty conflicts)
     (recur (-> strat-state
		(update-in [:rejected] union conflicts)
		(update-in [:considering] difference conflicts)))
     
     ;; don't continue 50% of the time
     (< 0.5 (rand)) strat-state

     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (not-empty unexplained)
     (let [hyp (rand-nth (vec unexplained))
	   explainers (get-explainers (:hypspace strat-state) hyp)]
       (if (empty? explainers)

	 ;; no explainers, just add the hyp
	 (recur (-> strat-state
		    (update-in [:accepted] conj hyp)))

	 ;; some explainers, add a random one and the hyp
	 (let [expl (rand-nth (vec explainers))]
	   (recur (-> strat-state
		      ;; TODO: does not support composite explainers
		      (update-in [:accepted] union #{hyp expl})
		      (update-in [:considering] difference #{hyp expl}))))))
     
     "default" strat-state)))

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
     "default" (explain-guess strat-state))))


