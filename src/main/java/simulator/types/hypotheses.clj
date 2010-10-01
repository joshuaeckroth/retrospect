(ns simulator.types.hypotheses
  (:use [clojure.set])
  (:use [simulator.confidences]))

(defprotocol Hypothesis
  (get-id [this])
  (get-apriori [this]))

(defn get-hyp-id-str
  [hyp]
  (str (get-id hyp)))

(defn get-hyp-ids-str
 [hyps]
 (apply str (interpose "," (map get-id hyps))))

(defrecord HypothesisSpace [hyps explains explainers conflicts confidence])

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
  [hypspace hyps es]
  "Record that each of 'hyps' is explained by each of 'es'."
  (reduce
   (fn [hs hyp]
     (let [origexplainers (get-explainers hs hyp)
           newexplainers (assoc (:explainers hs) hyp
                                (if origexplainers (union origexplainers es) es))]
       ;; add each 'explains' relationship...
       (reduce (fn [hs2 e] (add-single-explains hs2 e hyp))
               ;; after adding the explainers
               (assoc hs :explainers newexplainers)
               es)))
   hypspace hyps))

(defn get-conflicts
  [hypspace hyp]
  (get (:conflicts hypspace) hyp))

(defn add-conflicts
  [hypspace hyp cs]
  (let [origconflicts (get-conflicts hypspace hyp)
	newconflicts (assoc (:conflicts hypspace) hyp
			    (if origconflicts (union origconflicts cs) cs))]
    (assoc hypspace :conflicts newconflicts)))

(defn get-confidence
  [hypspace hyp]
  (get (:confidence hypspace) hyp))

(defn set-confidence
  [hypspace hyp c]
  (let [newconfidence (assoc (:confidence hypspace) hyp c)]
    (assoc hypspace :confidence newconfidence)))

(defn boost
  [hypspace hyp]
  (set-confidence hypspace hyp (max VERY-PLAUSIBLE (get-confidence hypspace hyp))))

(defn penalize
  [hypspace hyp]
  (set-confidence hypspace hyp (min VERY-IMPLAUSIBLE (get-confidence hypspace hyp))))

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
  "Returns a collection with items like {:hyp h :essential e}."
  (map (fn [{h :hyp es :explainers}] {:hyp h :essential (first es)})
       (filter #(= 1 (count (:explainers %)))
               (map (fn [h] {:hyp h :explainers (get-explainers hypspace h)}) hyps))))

(defn find-conflicts
  [hypspace hyps]
  (reduce union (map #(get-conflicts hypspace %) hyps)))

(defn find-best
  [hypspace hyps threshold]
  "Returns a collection with items like {:hyp h :best e :conf c}"
  (filter identity
	  (for [h hyps]
	    (let [explainers (get-explainers hypspace h)
		  expconf (map (fn [e] {:explainer e
                                        :conf (get-confidence hypspace e)})
                               explainers)
		  expsorted (reverse (sort-by :conf expconf))]
	      
	      (cond (empty? expsorted) nil

		    ;; single explainer or difference in confidence above a threshold?
                    (or
                     (= 1 (count expsorted))
                     (<= threshold (- (:conf (first expsorted))
                                      (:conf (second expsorted)))))
		    {:hyp h :best (:explainer (first expsorted))
		     :conf (:conf (first expsorted))}
		    
		    :else nil)))))

