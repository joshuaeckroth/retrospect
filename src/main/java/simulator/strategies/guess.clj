(ns simulator.strategies.guess
  (:use [simulator.types.states :only (get-entities)])
  (:use [simulator.strategies :only (add-log explain-new-entity explain-existing-entity)])
  (:use [simulator.types.generic :only (to-str)]))

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
			 (add-log time (str "Guessing spotted " (to-str (first spotted))
					    " is new entity"))
			 (explain-new-entity (first spotted) time)))
	      :else
	      (recur (rest spotted)
		     (-> s
			 (add-log time (str "Guessing spotted " (to-str (first spotted))
					   " is continuation of " (to-str (nth es choice))))
			 (explain-existing-entity (first spotted) (nth es choice) time))))))))

