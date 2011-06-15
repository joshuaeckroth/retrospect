(ns retrospect.problems.words.hypotheses
  (:use [retrospect.sensors :only [sensed-at]]))

(defn inconsistent
  [pdata hyps rejected]
  [])

(defn hypothesize
  [ep-state sensors time-now params]
  (println (sensed-at (first sensors) time-now))
  ep-state)

(defn get-more-hyps
  [ep-state]
  ep-state)

(defn commit-decision
  ([pdata accepted] pdata)
  ([pdata accepted alts] pdata))
