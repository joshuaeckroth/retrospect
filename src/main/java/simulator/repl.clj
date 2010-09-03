(ns simulator.repl
  (:use [clojure.contrib.str-utils2 :only (grep)])
  (:use [simulator.types.hypotheses]))

(def *strat-state*)

(defn update-strat-state
  [ss]
  (def *strat-state* ss))

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
  [time]
  (print-hyps (get (:hypothesized-at *strat-state*) time)))

(defn rejected-at
  [time]
  (print-hyps (get (:rejected *strat-state*) time)))

(defn accepted-at
  [time]
  (print-hyps (get (:accepted *strat-state*) time)))

(defn unexplained-before
  [time]
  (print-hyps (get (:unexplained-before *strat-state*) time)))

(defn unexplained-after
  [time]
  (print-hyps (get (:unexplained-after *strat-state*) time)))

(defn considering-before
  [time]
  (print-hyps (get (:considering-before *strat-state*) time)))

(defn considering-after
  [time]
  (print-hyps (get (:considering-after *strat-state*) time)))

(defn accepted
  []
  (doseq [time (sort (keys (:rejected *strat-state*)))]
    (println (format "* Accepted at %d *" time))
    (rejected-at time)))

(defn rejected
  []
  (doseq [time (sort (keys (:rejected *strat-state*)))]
    (println (format "* Rejected at %d *" time))
    (rejected-at time)))

(defn accepted-rejected
  []
  (let [maxtime (apply max (concat (keys (:rejected *strat-state*))
				   (keys (:accepted *strat-state*))))]
    (doseq [time (range 0 (inc maxtime))]
      (println (format "** Time %d **" time))
      (println (format "* Accepted at %d *" time))
      (accepted-at time)
      (println (format "* Rejected at %d *" time))
      (rejected-at time))))

(defn abducer-log-at
  [time]
  (doseq [msg (get (:abducer-log *strat-state*) time)]
    (println (str msg))))

(defn log-at
  [time]
  (doseq [msg (get (:log *strat-state*) time)]
    (println (str msg))))
