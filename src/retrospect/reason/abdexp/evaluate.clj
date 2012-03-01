(ns retrospect.reason.abdexp.evaluate
  (:use [retrospect.evaluate :only [calc-increase]])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.state]))

(defn evaluate
  [{:keys [expgraph least]} ors]
  (let [{:keys [arb efli]} (:workspace (cur-ep (:est ors)))]
    (update-in
     ors [:results] conj
     (merge {:Problem (:name @problem)}
            params
            {:ArbCardInc (- (cardinality arb) (cardinality least))
             :ArbComplete (complete? arb)
             :ArbEqualLeast (= arb least)
             :EFLICardInc (- (cardinality efli) (cardinality least))
             :EFLIComplete (complete? efli)
             :EFLIEqualLeast (= efli least)}))))

(defn evaluate-comp
  [control-results comparison-results control-params comparison-params]
  (apply merge (map #(calc-increase control-results comparison-results %)
                    [:ArbCardInc :EFLICardInc])))

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
