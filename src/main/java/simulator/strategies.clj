(ns simulator.strategies
  (:use simulator.types.hypotheses)
  (:use clojure.set))

(defn explain-guess
  [strat-state]
  (let [conflicts (difference
		   (find-conflicts (:hypspace strat-state) (:accepted strat-state))
		   (:rejected strat-state))
	unexplained (find-unexplained (:hypspace strat-state) (:accepted strat-state))]
    (cond
     
     ;; remove any conflicts
     (not-empty conflicts)
     (recur (-> strat-state
		(update-in [:rejected] union conflicts)
		(update-in [:considering] difference conflicts)))
     
     ;; don't continue 10% of the time
     (< (rand) 0.1) strat-state

     ;; choose an unexplained hyp and add a random explainer (if any exist)
     (not-empty unexplained)
     (let [hyp (rand-nth (vec unexplained))
	   explainers (difference (get-explainers (:hypspace strat-state) hyp)
				  (:rejected strat-state))]
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
     
     :else strat-state)))

(defn explain-essentials-guess
  [strat-state]
  (let [conflicts (difference
		   (find-conflicts (:hypspace strat-state) (:accepted strat-state))
		   (:rejected strat-state))
	unexplained (find-unexplained (:hypspace strat-state) (:accepted strat-state))
	essentials (find-essentials (:hypspace strat-state) unexplained)]
    (cond
     
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
     :else (explain-guess strat-state))))


