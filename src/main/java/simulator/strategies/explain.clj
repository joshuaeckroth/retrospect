(ns simulator.strategies.explain
  (:use [simulator.strategies.guess :only (explain-guess)])
  (:use [simulator.strategies.nearest :only (explain-nearest)]))

(defn explain
  [strat-state sensors time]
  (case (:strategy strat-state)
	"guess" (explain-guess strat-state sensors time)
	"nearest" (explain-nearest strat-state sensors time)))

