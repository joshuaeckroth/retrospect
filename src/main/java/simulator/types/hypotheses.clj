(ns simulator.types.hypotheses
  (:use clojure.set))

(defrecord HypothesisSpace [hyps explains explainers conflicts apriori])

(defn init-hypspace []
  ;; hyps must be a set, rest must be maps
  (HypothesisSpace. #{} {} {} {} {}))

(defn get-explainers
  [hypspace hyp]
  (get (:explainers hypspace) hyp))

(defn get-explains
  [hypspace hyp]
  (get (:explains hypspace) hyp))

(defn add-single-explains
  [hypspace explainer hyp]
  "Record that 'explainer' is an explanation of 'hyp'."
  (let [origexplains (get-explains hypspace explainer)
	newexplains (assoc (:explains hypspace) explainer
			   (if origexplains (conj origexplains hyp) #{hyp}))]
    (assoc hypspace :explains newexplains)))

(defn add-explainers
  [hypspace hyp es]
  "Record that 'hyp' is explained by each of 'es'."
  (let [origexplainers (get-explainers hypspace hyp)
	newexplainers (assoc (:explainers hypspace) hyp
			     (if origexplainers (union origexplainers es) es))]
    ;; add each 'explains' relationship...
    (reduce (fn [hs e] (add-single-explains hs e hyp))
	    ;; after adding the explainers
	    (assoc hypspace :explainers newexplainers)
	    es)))

(defn get-conflicts
  [hypspace hyp]
  (get (:conflicts hypspace) hyp))

(defn add-conflicts
  [hypspace hyp cs]
  (let [origconflicts (get-conflicts hypspace hyp)
	newconflicts (assoc (:conflicts hypspace) hyp
			    (if origconflicts (union origconflicts cs) cs))]
    (assoc hypspace :conflicts newconflicts)))

(defn get-apriori
  [hypspace hyp]
  (get (:apriori hypspace) hyp))

(defn set-apriori
  [hypspace hyp p]
  (let [newapriori (assoc (:apriori hypspace) hyp p)]
    (assoc hypspace :apriori newapriori)))

(defn explained?
  [hypspace hyp hyps]
  "Is 'hyp' explained by 'hyps'?"
  (let [explainers (get-explainers hypspace hyp)]
    (if (empty? explainers) true
	(loop [es explainers]
	  (cond (empty? es) false
		(set? (first es))
		(if (subset? (first es) hyps) true
		    (recur (rest es)))
		:else
		(if (some #(= % (first es)) hyps) true
		    (recur (rest es))))))))

(defn find-unexplained
  [hypspace hyps]
  (difference hyps (set (filter #(explained? hypspace % hyps) hyps))))

;; does not work for composite essentials
(defn find-essentials
  [hypspace hyps]
  (let [essentials (apply union (filter #(= 1 (count %))
					(map #(get-explainers hypspace %) hyps)))]
    (filter #(< 0.0 (get-apriori hypspace %)) essentials)))

(defn find-conflicts
  [hypspace hyps]
  (reduce union (map #(get-conflicts hypspace %) hyps)))

(defn find-best-by-threshold
  [hypspace hyps threshold]
  (filter identity
	  (for [h hyps]
	    (let [explainers (get-explainers hypspace h)
		  expapriori (map (fn [e] {:explainer e
					   :apriori (get-apriori hypspace e)})
				  explainers)
		  expsorted (sort-by :apriori expapriori)]
	      
	      (cond (empty? expsorted) nil

		    ;; single explainer or difference in apriori above a threshold?
		    (and
		     (< 0.0 (:apriori (first expsorted)))
		     (or
		      (= 1 (count expsorted))
		      (<= threshold (- (:apriori (first expsorted))
				       (:apriori (second expsorted))))))
		    {:hyp h :explainer (:explainer (first expsorted))
		     :apriori (:apriori (first expsorted))}
		    
		    :else nil)))))

(defn find-clearbest
  [hypspace hyps]
  (find-best-by-threshold hypspace hyps 0.5))

(defn find-weakbest
  [hypspace hyps]
  (find-best-by-threshold hypspace hyps 0.2))
