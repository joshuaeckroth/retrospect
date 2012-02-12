(ns retrospect.problems.words.problem
  (:use [retrospect.reason.abduction.problems.words.evaluate :only
         [evaluate evaluate-comp true-hyp? hyps-equal?]])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors perturb]])
  (:use [retrospect.reason.abduction.problems.words.hypotheses :only
         [hypothesize make-sensor-hyps generate-kb]])
  (:use [retrospect.problems.words.player :only
         [player-get-stats-panel player-update-stats player-get-truedata-log
          player-get-problem-log player-setup-diagram player-update-diagram]])
  (:use [retrospect.problems.words.prepared :only [prepared-map]]))

(def words-problem
  {:name "Words"
   :player-fns {:get-stats-panel-fn player-get-stats-panel
                :update-stats-fn player-update-stats
                :get-truedata-log player-get-truedata-log
                :get-problem-log player-get-problem-log
                :setup-diagram-fn player-setup-diagram
                :update-diagram-fn player-update-diagram}
   :generate-truedata-fn generate-truedata
   :generate-sensors-fn generate-sensors
   :perturb-fn perturb
   :prepared-map prepared-map
   :abduction {:generate-kb-fn generate-kb
               :make-sensor-hyps-fn make-sensor-hyps
               :hypothesize-fn hypothesize
               :evaluate-fn evaluate
               :evaluate-comp-fn evaluate-comp
               :true-hyp?-fn true-hyp?
               :hyps-equal?-fn hyps-equal?
               :hyp-types [:word :noise-word :word-seq :learned-word]
               :default-params
               {:MaxLearnedWords [10 [10]]
                :MaxNoisyWords [10 [10]]}}
   :default-params
   {:Steps [600 [600]]
    :StepsBetween [30 (range 10 101 30)]
    :SensorNoise [0 [0 5 10 15 20]]
    :Dataset ["cityu" ["cityu"]]
    :Encoding ["big5" ["big5"]]
    :LearnFeatureSize [2 (range 1 4)]
    :MaxModelGrams [3 (range 1 6)]
    :MinWordLength [3 (range 1 5)]
    :MinLearnLength [5 (range 5 10)]
    :MaxLearnLength [8 (range 8 13)]}})
