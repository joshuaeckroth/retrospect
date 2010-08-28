(ns simulator.types.hypotheses
  (:use clojure.set))

(defrecord HypothesisSpace [hyps explainers conflicts])

(defn get-explainers
  [hypspace hyp]
  (get (:explainers hypspace) hyp))

(defn set-explainers
  [hypspace hyp es]
  (let [newexplainers (assoc (:explainers hypspace) hyp es)]
    (assoc hypspace :explainers newexplainers)))

(defn get-conflicts
  [hypspace hyp]
  (get (:conflicts hypspace) hyp))

(defn set-conflicts
  [hypspace hyp cs]
  (let [newconflicts (assoc (:conflicts hypspace) hyp cs)]
    (assoc hypspace :conflicts newconflicts)))

(defn explained?
  [hypspace hyp hyps]
  "Is hyp explained by hyps?"
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
  (apply union (filter #(= 1 (count %)) (map #(get-explainers hypspace %) hyps))))

(defn find-conflicts
  [hypspace hyps]
  (reduce union (map #(get-conflicts hypspace %) hyps)))

