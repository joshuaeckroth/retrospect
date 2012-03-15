(ns retrospect.reason.abdexp.evaluate
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state]))

(defn conf-expgraph
  [expgraph]
  (if (empty? (accepted-nodes expgraph)) 0.0
      (/ (reduce + (map #(score expgraph %) (accepted-nodes expgraph)))
         (count (accepted-nodes expgraph)))))

(defn evaluate
  [_ ors]
  (let [{:keys [arb efli deltas]} (:workspace (cur-ep (:est ors)))]
    (update-in
     ors [:results] conj
     (merge {:Problem (:name @problem)}
            params
            {:ArbConf (conf-expgraph arb)
             :ArbCoverage (double (/ (count (data-explained-by-top arb))
                                     (count (data-nodes arb))))
             :EFLIConf (conf-expgraph efli)
             :EFLIDelta (if (empty? deltas) 1.0
                            (/ (reduce + (vals deltas))
                               (count deltas)))
             :EFLICoverage (double (/ (count (data-explained-by-top efli))
                                      (count (data-nodes efli))))}))))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:ArbConf :ArbCoverage :EFLIConf :EFLIDeltaConf :EFLICoverage])))

(comment
  (println "Nodes only in arbitrary:" (difference (filled-nodes eg-arb)
                                                  (filled-nodes eg-least)))
  (println "Nodes only in efli:" (difference (filled-nodes eg-efli)
                                             (filled-nodes eg-least)))
  (println "Nodes in least:" (difference (filled-nodes eg-least)
                                         (filled-nodes eg-arb)))
  (println "Least is complete?" (complete? eg-least))
  (println "--")
  (println "Arbitrary is complete?" (complete? eg-arb))
  (println "Arbitrary equal to least?" (= eg-arb eg-least))
  (println "Nodes in least-arb:"
           (difference (filled-nodes eg-least)
                       (filled-nodes eg-arb)))
  (println "Arbitrary cardinality increase:"
           )
  (println "--")
  (println "EFLI is complete?" (complete? eg-efli))
  (println "EFLI equal to least?" (= eg-efli eg-least))
  (println "Nodes in least-efli:"
           (difference (filled-nodes eg-least)
                       (filled-nodes eg-efli)))
  (println "EFLI cardinality increase:"
           (- (cardinality eg-efli) (cardinality eg-least)))
  (view eg-efli))
