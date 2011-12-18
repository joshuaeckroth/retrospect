(ns retrospect.test.utils
  (:require [retrospect.state])
  (:use [retrospect.random :only [rgen new-seed]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.problem :only [run-simulation]])
  (:use [retrospect.onerun :only [init-one-run-state]]))

(defn approx=
  "Return true if the absolute value of the difference between x and y
   is less than eps."
  [x y eps]
  (< (Math/abs (- x y)) eps))

(defn run
  [problem {:keys [params sensors truedata]}]
  (dosync
   (alter retrospect.state/datadir (constantly "data"))
   (alter retrospect.state/problem (constantly problem)))
  (binding [rgen (new-seed (:Seed params))
            last-id 0
            retrospect.state/params params]
    (let [problem-data ((:gen-problem-data-fn problem) truedata sensors)
          or-state (init-one-run-state sensors problem-data)]
      (run-simulation truedata or-state false))))

