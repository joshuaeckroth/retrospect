(ns simulator.player.repl
  (:use [clojure.contrib.str-utils2 :only (grep)])
  (:use [simulator.epistemicstates :only
         [print-ep-state-tree]])
  (:use [simulator.player.state]))

(defn lookup-hyp
  [hypid]
  (let [candidates (filter #(= hypid (:id %))
                           (:hyps (:hypspace (:workspace (:ep-state *or-state*)))))]
    (if (empty? candidates) nil (first candidates))))

(defn print-hyps
  [hyps]
  (doseq [h hyps] (println (str h))))

(defn list-hyps []
  (print-hyps (:hyps (:hypspace (:workspace (:ep-state *or-state*))))))

(defn hyp-log
  [hypid]
  (doseq [entry (get (:hyp-log (:workspace (:ep-state *or-state*))) (lookup-hyp hypid))]
    (println (str entry))))

(defn explains
  [hypid]
  (print-hyps (get-explains (:hypspace (:workspace (:ep-state *or-state*)))
                            (lookup-hyp hypid))))

(defn explainers
  [hypid]
  (print-hyps (get-explainers (:hypspace (:workspace (:ep-state *or-state*)))
                              (lookup-hyp hypid))))

(defn conflicts
  [hypid]
  (print-hyps (get-conflicts (:hypspace (:workspace (:ep-state *or-state*)))
                             (lookup-hyp hypid))))

(defn grep-hyps
  [re]
  (print-hyps (grep re (:hyps (:hypspace (:workspace (:ep-state *or-state*)))))))

(defn hypothesized
  []
  (print-hyps (:hypothesized (:workspace (:ep-state *or-state*)))))

(defn accepted
  []
  (print-hyps (:accepted (:workspace (:ep-state *or-state*)))))

(defn unexplained
  []
  (print-hyps (:unexplained (:workspace (:ep-state *or-state*)))))

(defn abducer-log
  []
  (doseq [msg (:abducer-log (:workspace (:ep-state *or-state*)))]
    (println (str msg))))

(defn log
  []
  (doseq [msg (:log (:ep-state *or-state*))]
    (println (str msg))))

(defn ep-state-tree
  []
  (print-ep-state-tree (:ep-state-tree *or-state*)))

(defn change-ep-state
  [id]
  (let [ep-tree (goto-ep-state (:ep-state-tree *or-state*) id)]
    (update-or-state
     (-> *or-state*
         (assoc :ep-state-tree ep-tree)
         (assoc :ep-state (current-ep-state ep-tree))))))