(ns simulator.strategies.nearest
  (:use [simulator.types.positions :only (manhattan-distance)])
  (:use [simulator.types.states :only (get-entities)])
  (:use [simulator.strategies.core :only (add-log explain-new-entity explain-existing-entity)])
  (:use [simulator.types.entities :only (pos)])
  (:use [simulator.types.generic :only (to-str)]))

(defn pair-nearest
  "This is an instance of the closest pairs problem. Note that, at the moment,
   the brute-force algorithm is used, which has complexity O(n^2)."
  [spotted entities]
  (let [pairs-of-pairs (for [s spotted]
			 (for [e entities]
			   {:spotted s :entity e :dist (manhattan-distance (pos s) (pos e))}))
	sorted-pairs (sort-by :dist (apply concat pairs-of-pairs))]
    (for [s spotted] (first (filter #(= (:spotted %) s) sorted-pairs)))))

;; 'new-entities' are always incorrect?
(defn explain-nearest
  "The idea behind 'explain-nearest' is all spotted & existing entities will be
   paired according to k-closest pair (where k = number of spotted entities),
   and given these pairings, each spotted entity whose pairing has a distance
   less than some constant will be explained as having moved from its paired
   existing entity; all pairings with too great a distance will have 'new-entity'
   explanations."
  [strat-state sensors time]
  (let [unique-spotted (set (apply concat (map :spotted sensors)))]
    (if (empty? (get-entities strat-state))
      (reduce (fn [s spotted]
		(-> s
		    (add-log time (str "Explaining " (to-str spotted) " as new."))
		    (explain-new-entity spotted time)))
	      strat-state unique-spotted)
      (loop [pairs (pair-nearest unique-spotted (get-entities strat-state))
	     s strat-state]
	(cond (empty? pairs) s
	      (> (:dist (first pairs)) 5)
	      (recur (rest pairs)
		     (-> s
			 (add-log time (str "Explaining " (to-str (:spotted (first pairs)))
					   " as new since its distance to next-nearest is > 5"))
			 (explain-new-entity (:spotted (first pairs)) time)))
	      :else
	      (recur (rest pairs)
		     (-> s
			 (add-log time (str "Explaining " (to-str (:spotted (first pairs)))
					   " as continuation of next-nearest "
					   (to-str (:entity (first pairs)))))
			 (explain-existing-entity
			  (:spotted (first pairs))
			  (:entity (first pairs)) time))))))))

