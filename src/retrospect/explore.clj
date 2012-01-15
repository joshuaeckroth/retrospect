(ns retrospect.explore
  (:use [retrospect.random :only [rgen new-seed my-rand-nth my-rand-int my-rand]])
  (:use [retrospect.problem :only [get-default-params-ranges run-simulation]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.state]))

;; the following is from:
;; https://github.com/clojure/clojurescript/blob/master/samples
;;   /twitterbuzz/src/twitterbuzz/anneal.cljs
;; with modifications

; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(defn standard-prob
  [e e1 comp temp]
  (if (comp e1 e) 1
      (Math/exp (/ (Math/abs (- e e1)) temp))))

(defn linear-cooling
  [steps]
  (let [min (- 1 (/ steps (dec steps)))]
    (fn [t] (max min (- 1 (/ t steps))))))

(def attempted (atom {}))

(defn anneal
  "Given an energy scoring function, a temperature function
   (calculates temp given iteration t), a permutation
   function (creates a candidate next state given a current state),
   and an acceptance probability function (of last next energy and
   temp), yields a lazy seq of (accepted?) states of the form {:state
   s :score :best best :best-score :t t}"

  [energy  ;;(energy state) -> score
   temp    ;;(temp t) -> 0-1.0
   permute ;;(permute state) -> new-state
   prob    ;;(prob e e1 temp) -> 0-1.0
   state
   min-max]

  (let [init state
        init-score (energy state)
        comp (if (= "min" min-max) < >)
        opt (if (= "min" min-max) min max)
        step (fn step [{:keys [state score best best-score t] :as ret}]
               (loop [next (permute state) t (inc t)]
                 (let [next-score (energy next)
                       select? (> (prob score next-score comp (temp t)) (my-rand))]
                   (println "Select?" select? "/ temp is" (double (temp t)))
                   (if select?
                     (let [ret {:state next :score next-score :t t
                                :best (if (comp next-score best-score) next best)
                                :best-score (opt next-score best-score)}]
                       (when (comp next-score best-score)
                         (println "New best score:" (:best-score ret)
                                  "for" (:state ret)))
                       (lazy-seq (cons ret (step ret))))
                     (recur (permute state) (inc t))))))]
    (step {:state init :score init-score :best init :best-score init-score :t 0})))

;; end external clojure code

(defn run
  [metric params repetitions]
  (println "Simulating" (pr-str params))
  (let [rs (for [i (range repetitions)]
             (let [seed (my-rand-int 10000000)]
               (binding [rgen (new-seed seed)
                         last-id 0
                         retrospect.state/params (assoc params :Seed seed)]
                 (println "Seed:" seed)
                 (let [truedata ((:truedata-fn @problem))
                       sensors ((:sensor-gen-fn @problem))
                       problem-data ((:gen-problem-data-fn @problem) truedata sensors)
                       or-state (init-one-run-state sensors problem-data)
                       results (run-simulation truedata or-state false)]
                   (println (format "%s = %s" (name metric) (get (last results) metric)))
                   results))))
        avg (/ (reduce + (map #(get (last %) metric) rs)) repetitions)]
    (swap! attempted assoc params rs)
    (println "Average =" avg)
    avg))

(defn next-params
  [def-ps last-params]
  (loop []
    (let [field (my-rand-nth (filter #(second (get def-ps %)) (keys def-ps)))
          val (my-rand-nth (get def-ps field))
          params (assoc last-params field val)]
      (println "\nSwapping" field "with" val "\n")
      (if (or (= (get last-params field) val) (some #{params} (keys @attempted)))
        (recur) params))))

(defn explore
  [seed metric min-max repetitions]
  (binding [rgen (new-seed seed)]
    (let [def-ps (get-default-params-ranges)
          first-ps (reduce (fn [m k] (assoc m k (my-rand-nth (get def-ps k))))
                           {} (keys def-ps))]
      (anneal #(run metric % repetitions)
              (linear-cooling 100)
              (partial next-params def-ps)
              standard-prob
              first-ps
              min-max))))
