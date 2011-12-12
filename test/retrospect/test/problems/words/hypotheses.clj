(ns retrospect.test.problems.words.hypotheses
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.workspaces :only [last-id new-hyp]])
  (:use [retrospect.state])
  (:use [retrospect.problems.words.hypotheses]))

(defn get-words
  [hyps]
  (set (map (comp :words :data) hyps)))

(deftest grab-bag
  (binding [last-id 0
            params {:BelievedKnowledge 80}]
    (let [truedata "goobarquuxxyz"
          models {1 (with-meta {["foo"] 1 ["bar"] 1 ["quux"] 1} {:sum 3})
                  2 (with-meta {["foo" "bar"] 1 ["bar" "quux"] 1} {:sum 2})
                  3 (with-meta {["foo" "bar" "quux"] 1} {:sum 1})}
          dictionary #{"foo" "bar" "quux"}
          sensor-hyps (map (fn [i] (new-hyp "Sens" :sensor nil 1.0 nil [] "" {:pos i}))
                           (range (count truedata)))
          letters (seq truedata)
          indexed-letters (make-indexed-letters letters)
          word-hyps-nonoise (make-word-hyps indexed-letters 0 dictionary
                                            0.0 sensor-hyps models)
          word-hyps-noise (make-word-hyps indexed-letters 0 dictionary
                                          0.1 sensor-hyps models)
          noise-hyps (filter #(not= 0 (:changes (:data %))) word-hyps-noise)
          composite-hyps-nonoise (make-composite-hyps models word-hyps-nonoise #{} 3)
          composite-hyps-noise (make-composite-hyps models word-hyps-noise #{} 3)
          learning-hyps (make-learning-hyps indexed-letters [0 1 2 10 11 12]
                                            0 dictionary sensor-hyps)]
      (is (= #{["bar"] ["quux"]} (get-words word-hyps-nonoise)))
      (is (= #{["foo"]} (get-words noise-hyps)))
      (is (= #{["bar" "quux"]} (get-words composite-hyps-nonoise)))
      (is (= #{["foo" "bar"] ["bar" "quux"] ["foo" "bar" "quux"]}
             (get-words composite-hyps-noise)))
      (is (= #{["goo"] ["xyz"]} (get-words learning-hyps))))))

