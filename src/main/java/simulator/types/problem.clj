(ns simulator.types.problem
  (:use [simulator.strategies.core :only (init-strat-state strategies)]))

;;(defn start-player [problem] ((:player-fn problem)))

(defn average-single-run
  [problem strategy params n]
  (let [runs (for [i (range n)] ((:runner-fn problem) params (init-strat-state strategy)))
	result (first runs) ;; choose any result; its 'avg-fields' will be updated
	avg (fn [field rs] (double (/ (reduce + (map field rs)) n)))]
    (apply assoc result (zipmap (:avg-fields problem) (map #(avg % runs) (:avg-fields problem))))))

(defn average-some-runs
  [problem params n]
  (apply concat (for [strategy strategies]
		  (doall (map #(average-single-run problem strategy % n) params)))))

(defn get-headers [problem] (:headers problem))

(defrecord Problem
  [name runner-fn headers avg-fields non-avg-fields])
