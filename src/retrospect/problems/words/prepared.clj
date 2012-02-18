(ns retrospect.problems.words.prepared
  (:require [clojure.string :as str])
  (:use [retrospect.problems.words.truedata :only [generate-truedata]])
  (:use [retrospect.problems.words.sensors :only [generate-sensors]])
  (:use [retrospect.problems.words.symbols])
  (:use [retrospect.state :only [params]]))

(defn simple
  []
  (let [training-sentences
        (map #(str/split % #"\s+")
             ["really , dinah ought to have taught you better manners !"
              "just as if some one was kissing the window all over outside ."
              "cried alice , dropping the ball of worsted to clap her hands ."
              "kitty can dont smile you play my dear chess"])
        test-sentences
        (map #(str/split % #"\s+")
             ["kitty , can you play chess ?"
              "now , dont smile , my dear , im asking it seriously ."])
        ambiguous (map #(apply str %) test-sentences)]
    {:params {:Steps 2 :MaxModelGrams 3 :LearnFeatureSize 2}
     :truedata {:training [training-sentences
                           (set (filter #(not (re-matches punctuation-regex %))
                                        (apply concat training-sentences)))]
                :test (zipmap (range (count ambiguous)) ambiguous)
                :test-sentences test-sentences
                :test-dict (set (filter #(not (re-matches punctuation-regex %))
                                        (apply concat test-sentences)))}
     :sensors (generate-sensors)}))

(def prepared-map
     (sorted-map "simple" simple))
