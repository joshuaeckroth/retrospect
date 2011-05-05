(ns retrospect.test-workspaces
  (:use midje.sweet)
  (:use retrospect.workspaces)
  (:use retrospect.confidences))

(fact (:confidence (init-workspace)) => nil)

(binding [last-id 0]
  (let [hyp (new-hyp "TEST" :test NEUTRAL [] "No desc." nil)
        ws (add (init-workspace) hyp [] :static)]
    (fact (get-hyps ws) => (just hyp))))
