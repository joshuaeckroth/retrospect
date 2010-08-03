(ns simulator.strategies.guess
  (:use [simulator.types.states :only (addLog)])
  (:use [simulator.types.generic :only (toStr)])
  (:use [simulator.explain :only (explain-new-entity explain-existing-entity)]))

(defn explain-guess
  [sensors strat-state time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))
	es (:entities strat-state)
	numes (count es)]
    (loop [spotted unique-spotted
	   state strat-state]
      (let [choice (rand-int (inc numes))]
	(cond (empty? spotted) state
	      (= choice numes)
	      (recur (rest spotted)
		     (-> state
			 (addLog time (str "Guessing spotted " (toStr (first spotted)) " is new entity"))
			 (explain-new-entity (first spotted) time)))
	      :else
	      (recur (rest spotted)
		     (-> state
			 (addLog time (str "Guessing spotted " (toStr (first spotted))
					   " is continuation of " (toStr (nth es choice))))
			 (explain-existing-entity (first spotted) (nth es choice) time))))))))
