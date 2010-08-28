(ns simulator.strategies.guess
  (:use clojure.set)
  (:use [simulator.problems.tracking.states :only (get-entities)])
  (:use [simulator.strategies.core :only (add-log explain-new-entity explain-existing-entity)]))

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
     (let [hyp (rand-nth unexplained)
	   explainers (get-explainers (:hypspace strat-state) hyp)]
       (if (empty? explainers)

	 ;; no explainers, just add the hyp
	 (recur (-> strat-state
		    (update-in [:accepted] conj hyp)))

	 ;; some explainers, add a random one and the hyp
	 (let [expl (rand-nth explainers)]
	   (recur (-> strat-state
		      ;; TODO: does not support composite explainers
		      (update-in [:accepted] union #{hyp expl})
		      (update-in [:considering] difference #{hyp expl})))))))))

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

(defn explain-guess
  [strat-state sensors time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))
	es (get-entities strat-state)
	numes (count es)]
    (loop [spotted unique-spotted
	   s strat-state]
      (let [choice (rand-int (inc numes))]
	(cond (empty? spotted) s
	      (= choice numes)
	      (recur (rest spotted)
		     (-> s
			 (add-log time (str "Guessing spotted " (first spotted)
					    " is new entity"))
			 (explain-new-entity (first spotted) time)))
	      :else
	      (recur (rest spotted)
		     (-> s
			 (add-log time (str "Guessing spotted " (first spotted)
					    " is continuation of " (nth es choice)))
			 (explain-existing-entity (first spotted) (nth es choice) time))))))))

