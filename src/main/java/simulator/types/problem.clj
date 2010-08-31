(ns simulator.types.problem
  (:use [simulator.strategies :only (init-strat-state strategies)]))

(defn start-player [problem] ((:player-fn problem)))

(defn average-single-run
  [problem strategy params n]
  (let [runs (for [i (range n)]
	       ((:runner-fn problem) params
		(init-strat-state strategy (:problem-data problem))))
	result (:results (first runs)) ;; choose any result; 'avg-fields' will be updated
	avg (fn [field] (double (/ (reduce + (map (comp field :results) runs)) n)))
	newfields (interleave (:avg-fields problem)
			      (map #(avg %) (:avg-fields problem)))]
    (apply assoc result newfields)))

(defn average-some-runs
  [problem params n]
  (apply concat (for [strategy strategies]
		  (doall (map #(average-single-run problem strategy % n) params)))))

(defn get-headers [problem] (:headers problem))

(defrecord Problem
  [name runner-fn player-fn headers avg-fields non-avg-fields problem-data])
