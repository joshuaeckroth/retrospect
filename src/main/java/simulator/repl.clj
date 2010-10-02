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

(defn hyp-log
  [hypid]
  (doseq [entry (get (:hyp-log *strat-state*) (lookup-hyp hypid))]
    (println (str entry))))

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

(defn accepted-at
  [time]
  (print-hyps (get (:accepted *strat-state*) time)))

(defn unexplained-before
  [time]
  (print-hyps (get (:unexplained-before *strat-state*) time)))

(defn unexplained-after
  [time]
  (print-hyps (get (:unexplained-after *strat-state*) time)))

(defn accepted
  []
  (doseq [time (sort (keys (:accepted *strat-state*)))]
    (println (format "* Accepted at %d *" time))
    (accepted-at time)))

(defn abducer-log-at
  [time]
  (doseq [msg (get (:abducer-log *strat-state*) time)]
    (println (str msg))))

(defn log-at
  [time]
  (doseq [msg (get (:log *strat-state*) time)]
    (println (str msg))))
