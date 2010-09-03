(ns simulator.repl
  (:use [clojure.contrib.str-utils2 :only (grep)])
  (:use [simulator.types.hypotheses]))

(def *strat-state*)

(defn update-strat-state
  [newss]
  (def *strat-state* newss))

(defn print-hyps
  [hyps]
  (doseq [h hyps] (println h)))

(defn list-hyps []
  (print-hyps (:hyps (:hypspace *strat-state*))))

(defn interrogate
  [hyp])

(defn explains
  [hyp])

(defn explainers
  [hyp]
  (print-hyps (get-explainers (:hypspace *strat-state*) hyp)))

(defn conflicts
  [hyp]
  (print-hyps (get-conflicts (:hypspace *strat-state*) hyp)))

(defn grep-hyps
  [re]
  (grep re (:hyps (:hypspace *strat-state*))))

(defn hypothesized-at
  [time])

(defn rejected-at
  [time])

(defn accepted-at
  [time])

(defn unexplained-before
  [time])

(defn unexplained-after
  [time])

(defn considering-before
  [time])

(defn considering-after
  [time])

