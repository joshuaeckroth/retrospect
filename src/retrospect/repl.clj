(ns retrospect.repl
  (:use [retrospect.player :only [start-player]])
  (:use [retrospect.problems.tracking.problem :only [tracking-problem]]))

(defn tracking-player
  []
  (start-player tracking-problem :repl true))
