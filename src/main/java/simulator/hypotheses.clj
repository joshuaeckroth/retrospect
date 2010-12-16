(ns simulator.hypotheses
  (:use [clojure.set])
  (:use [simulator.confidences]))

(defrecord HypothesisSpace [hyps explains explainers conflicts confidence])

(defn init-hypspace []
  ;; hyps must be a set, rest must be maps
  (HypothesisSpace. #{} {} {} {} {}))

(defn prob-apriori
  [prob]
  (cond
   (<= prob 20) VERY-IMPLAUSIBLE
   (<= prob 40) IMPLAUSIBLE
   (<= prob 60) NEUTRAL
   (<= prob 80) PLAUSIBLE
   :else VERY-PLAUSIBLE))

(defn prob-neg-apriori
  [prob]
  (prob-apriori (- 100 prob)))

(defprotocol Hypothesis
  (get-id [this])
  (get-apriori [this]))

(defn get-hyp-id-str
  [hyp]
  (str (get-id hyp)))

(defn get-hyp-ids-str
 [hyps]
 (apply str (interpose "," (map get-id hyps))))

(defn get-explainers
  [hypspace hyp]
  (set (get (:explainers hypspace) hyp)))

(defn get-explains
  [hypspace hyp]
  (set (get (:explains hypspace) hyp)))

(defn add-single-explains
  [hypspace explainer hyp]
  "Record that 'explainer' is an explanation of 'hyp'."
  (let [origexplains (get (:explains hypspace) explainer)
	newexplains (assoc (:explains hypspace) explainer
			   (if origexplains (conj origexplains hyp) #{hyp}))]
    (assoc hypspace :explains newexplains)))

(defn add-explainers
  [hypspace hyps es]
  "Record that each of 'hyps' is explained by each of 'es'."
  (reduce
   (fn [hs hyp]
     (let [origexplainers (get (:explainers hs) hyp)
           newexplainers (assoc (:explainers hs) hyp
                                (if origexplainers (concat origexplainers es) es))]
       ;; add each 'explains' relationship...
       (reduce (fn [hs2 e] (add-single-explains hs2 e hyp))
               ;; after adding the explainers
               (assoc hs :explainers newexplainers)
               es)))
   hypspace hyps))

(defn get-conflicts
  [hypspace hyp]
  (set (get (:conflicts hypspace) hyp)))

(defn add-conflicts
  [hypspace hyp cs]
  (let [origconflicts (get (:conflicts hypspace) hyp)
	newconflicts (assoc (:conflicts hypspace) hyp
			    (if origconflicts (concat origconflicts cs) cs))]
    (assoc hypspace :conflicts newconflicts)))

(defn get-confidence
  [hypspace hyp]
  (get (:confidence hypspace) hyp))

(defn set-confidence
  [hypspace hyp c]
  (let [newconfidence (assoc (:confidence hypspace) hyp c)]
    (assoc hypspace :confidence newconfidence)))

(defn remove-mapped-hyps
  [m hyps]
  "Removes hyps that appear in a sequence for some map key, as well as
   keys representing the hyps to be deleted."
  (let [m-without-keys (reduce (fn [mm h] (dissoc mm h)) m hyps)]
    (reduce (fn [mm h] (assoc mm h (filter (fn [hh] (not (some #(= % hh) hyps)))
                                           (get m-without-keys h))))
            {} (keys m-without-keys))))

(defn delete-hyps
  [hypspace hyps]
  (-> hypspace
      (assoc :hyps (apply disj (set (:hyps hypspace)) hyps))
      (assoc :explains (apply dissoc (:explains hypspace) hyps))
      (assoc :explainers (remove-mapped-hyps (:explainers hypspace) hyps))
      (assoc :conflicts (remove-mapped-hyps (:conflicts hypspace) hyps))
      (assoc :confidence (apply dissoc (:confidence hypspace) hyps))))

(defn boost
  [hypspace hyp]
  (set-confidence hypspace hyp (max VERY-PLAUSIBLE (get-confidence hypspace hyp))))

(defn penalize
  [hypspace hyp]
  (set-confidence hypspace hyp (min VERY-IMPLAUSIBLE (get-confidence hypspace hyp))))

(defn explained?
  [hypspace hyp hyps]
  "Is 'hyp' explained by 'hyps'? Does not support composite explainers."
  (let [explainers (get (:explainers hypspace) hyp)]
    (if (or (nil? explainers) (empty? explainers)) true ;; no explainers == explained
	(loop [es explainers]
	  (if (empty? es) false
              (if (some #(= % (first es)) hyps) true
                  (recur (rest es))))))))

(defn find-unexplained
  [hypspace hyps]
  (difference (set hyps) (set (filter #(explained? hypspace % hyps) hyps))))

;; does not work for composite essentials
(defn find-essentials
  [hypspace hyps]
  "Returns a collection with items like {:hyp h :essential e}."
  (doall (map (fn [{h :hyp es :explainers}] {:hyp h :essential (first es)})
              (filter #(= 1 (count (:explainers %)))
                      (doall (map (fn [h] {:hyp h
                                           :explainers (get-explainers hypspace h)})
                                  hyps))))))

(defn find-conflicts
  [hypspace hyps]
  (reduce union (doall (map #(get-conflicts hypspace %) hyps))))

(defn find-best
  [hypspace hyps threshold type]
  "Returns a collection with items like {:hyp h :best e :conf c}"
  (filter identity
	  (doall (for [h hyps]
                   (let [explainers (get-explainers hypspace h)
                         expconf (doall (map (fn [e] {:explainer e
                                                      :conf (get-confidence hypspace e)})
                                             explainers))
                         expsorted (reverse (sort-by :conf expconf))]
	      
                     (cond (empty? expsorted) nil

                           ;; single explainer or difference in confidence
                           ;; above a threshold?
                           (or
                            (= 1 (count expsorted))
                            (<= threshold (- (:conf (first expsorted))
                                             (:conf (second expsorted)))))
                           {:hyp h :best (:explainer (first expsorted))
                            :conf (:conf (first expsorted)) :type "best"}
		    
                           ;; if type is :smartbest and there is no true best,
                           ;; check for best by (threshold-1) and explains more
                           (and (<= (dec threshold) (- (:conf (first expsorted))
                                                       (:conf (second expsorted))))
                                (> (count (get-explainers
                                           hypspace
                                           (:explainer (first expsorted))))
                                   (count (get-explainers
                                           hypspace
                                           (:explainer (second expsorted))))))
                           {:hyp h :best (:explainer (first expsorted))
                            :conf (:conf (first expsorted)) :type "smartbest"}

                           :else nil))))))
