(ns samre.problems.tracking.evaluate
  (:use [samre.epistemicstates :only (current-ep-state)])
  (:use [samre.workspaces :only [lookup-hyps]])
  (:use [samre.confidences])
  (:require [clojure.set :as set]))

(defn evaluate
  [ep-state sensors truedata params]
  {})

