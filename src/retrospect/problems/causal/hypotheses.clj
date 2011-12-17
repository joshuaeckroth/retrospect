(ns retrospect.problems.causal.hypotheses)

(def compute 0)
(def memory 0)

(defn hypothesize
  [ep-state sensors time-now])

(defn commit-decision
  [pdata accepted rejected unaccepted time-now])

(defn retract
  [pdata hyp])

(defn no-explainer-hyps
  [hyps pdata])
