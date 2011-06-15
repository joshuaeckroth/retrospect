(ns retrospect.problems.words.evaluate)

(defn evaluate
  [ep-state results prev-ep sensors truedata params]
  {:PWC 0.0})

(defn evaluate-meta
  [ep-state meta-ep-state meta-accepted-type results truedata params]
  {:AvgMetaDiffPWC 0.0})

(defn evaluate-comparative
  [params [m b]]
  {:MetaPWC 0.0
   :BasePWC 0.0
   :RatioPWC 0.0
   :IncreasePWC 0.0})
