(ns simulator.repl
  (:use [clojure.contrib.str-utils2 :only (grep)])
  (:use [simulator.hypotheses]))

(def *strat-state*)

(defn update-strat-state
  [ss]
  (def *strat-state* ss))

(defn lookup-hyp
  [hypid]
  (let [candidates (filter #(= hypid (:id %))
                           (:hyps (:hypspace (:ep-state *strat-state*))))]
    (if (empty? candidates) nil (first candidates))))

(defn print-hyps
  [hyps]
  (doseq [h hyps] (println (str h))))

(defn list-hyps []
  (print-hyps (:hyps (:hypspace (:ep-state *strat-state*)))))

(defn hyp-log
  [hypid]
  (doseq [entry (get (:hyp-log (:ep-state *strat-state*)) (lookup-hyp hypid))]
    (println (str entry))))

(defn explains
  [hypid]
  (print-hyps (get-explains (:hypspace (:ep-state *strat-state*)) (lookup-hyp hypid))))

(defn explainers
  [hypid]
  (print-hyps (get-explainers (:hypspace (:ep-state *strat-state*)) (lookup-hyp hypid))))

(defn conflicts
  [hypid]
  (print-hyps (get-conflicts (:hypspace (:ep-state *strat-state*)) (lookup-hyp hypid))))

(defn grep-hyps
  [re]
  (print-hyps (grep re (:hyps (:hypspace (:ep-state *strat-state*))))))

(defn hypothesized
  []
  (print-hyps (:hypothesized (:ep-state *strat-state*))))

(defn accepted
  []
  (print-hyps (:accepted (:ep-state *strat-state*))))

(defn unexplained
  []
  (print-hyps (:unexplained (:ep-state *strat-state*))))

(defn abducer-log
  []
  (doseq [msg (:abducer-log (:ep-state *strat-state*))]
    (println (str msg))))

(defn log
  []
  (doseq [msg (:log (:ep-state *strat-state*))]
    (println (str msg))))
