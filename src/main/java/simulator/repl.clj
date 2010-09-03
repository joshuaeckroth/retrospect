(ns simulator.repl
  (:use [clojure.contrib.str-utils2 :only (grep)])
  (:use [simulator.types.hypotheses]))

(def *strat-state*)

(defn update-strat-state
  [newss]
  (def *strat-state* newss))

(defn lookup-hyp
  [hypid]
  (let [candidates (filter #(= hypid (:id %)) (:hyps (:hypspace *strat-state*)))]
    (if (empty? candidates) nil (first candidates))))

(defn print-hyps
  [hyps]
  (doseq [h hyps] (println (str h))))

(defn list-hyps []
  (print-hyps (:hyps (:hypspace *strat-state*))))

(defn interrogate
  [hypid])

(defn explains
  [hypid]
  (print-hyps (get-explains (:hypspace *strat-state*) (lookup-hyp hypid))))

(defn explainers
  [hypid]
  (print-hyps (get-explainers (:hypspace *strat-state*) (lookup-hyp hypid))))

(defn conflicts
  [hypid]
  (print-hyps (get-conflicts (:hypspace *strat-state*) (lookup-hyp hypid))))

(defn grep-hyps
  [re]
  (print-hyps (grep re (:hyps (:hypspace *strat-state*)))))

(defn hypothesized-at
  [time])

(defn rejected-at
  [time]
  (print-hyps (get (:rejected *strat-state*) time)))

(defn accepted-at
  [time]
  (print-hyps (get (:accepted *strat-state*) time)))

(defn unexplained-before
  [time])

(defn unexplained-after
  [time])

(defn considering-before
  [time])

(defn considering-after
  [time])

(defn accepted
  []
  (doseq [time (sort (keys (:rejected *strat-state*)))]
    (println (format "Accepted at %d:" time))
    (rejected-at time)))

(defn rejected
  []
  (doseq [time (sort (keys (:rejected *strat-state*)))]
    (println (format "Rejected at %d:" time))
    (rejected-at time)))

