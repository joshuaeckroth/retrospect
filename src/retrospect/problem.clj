(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.onerun :only
         [init-one-run-states update-one-run-state proceed-one-run-state]])
  (:use [retrospect.epistemicstates :only
         [explain previous-ep-state current-ep-state]])
  (:use [retrospect.meta.explain :only [explain-meta]])
  (:use [retrospect.sensors :only [update-sensors]]))

(defn evaluate
  [problem truedata or-state params]
  (let [prev-ep (previous-ep-state (:ep-state-tree or-state))
        ep-state (current-ep-state (:ep-state-tree or-state))
        res (:resources or-state)]
    (update-in or-state [:results] conj
               (merge ((:evaluate-fn problem)
                       (:ep-state or-state)
                       (:results or-state)
                       (previous-ep-state (:ep-state-tree or-state))
                       (:sensors or-state) truedata params)
                      (assoc params
                        :Step (:time (:ep-state or-state))
                        :MetaAbduction (:meta-abduction or-state)
                        :Lazy (:lazy or-state)
                        :MetaAbductions (:meta-abductions res)
                        :MetaAcceptedBad (:meta-accepted-bad res)
                        :MetaAcceptedImpossible (:meta-accepted-impossible res)
                        :MetaAcceptedImpossibleLconf (:meta-accepted-impossible-lconf res)
                        :MetaAcceptedNone (:meta-accepted-none res)
                        :Milliseconds (:milliseconds res)
                        :Unexplained
                        (if prev-ep (count (:unexplained (:workspace prev-ep))) 0)
                        ;; ExplainCycles are stored in current ep-state
                        :ExplainCycles
                        (:explain-cycles (:resources (:workspace ep-state)))
                        :HypothesisCount
                        (:hyp-count (:resources (:workspace prev-ep)))
                        :HypothesesNew
                        (:hyps-new (:resources (:workspace prev-ep))))))))

(defn calc-percent-increase
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (* 100.0 (/ (- (k m) (k b)) (k b))))))

(defn calc-ratio
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (/ (k m) (k b)))))

(defn evaluate-comparative
  [problem params [m b]]
  (merge ((:evaluate-comparative-fn problem) params [m b])
         {:MetaAbductions (:MetaAbductions m)
          :MetaAcceptedBad (:MetaAcceptedBad m)
          :MetaAcceptedImpossible (:MetaAcceptedImpossible m)
          :MetaAcceptedImpossibleLconf (:MetaAcceptedImpossibleLconf m)
          :MetaAcceptedNone (:MetaAcceptedNone m)
          :MetaMilliseconds (:Milliseconds m)
          :BaseMilliseconds (:Milliseconds b)
          :RatioMilliseconds (calc-ratio :Milliseconds m b)
          :IncreaseMilliseconds (calc-percent-increase :Milliseconds m b)
          :MetaUnexplained (:Unexplained m)
          :BaseUnexplained (:Unexplained b)
          :RatioUnexplained (calc-ratio :Unexplained m b)
          :MetaExplainCycles (:ExplainCycles m)
          :BaseExplainCycles (:ExplainCycles b)
          :RatioExplainCycles (calc-ratio :ExplainCycles m b)
          :IncreaseExplainCycles (calc-percent-increase :ExplainCycles m b)
          :Steps (:Steps params)
          :StepsBetween (:StepsBetween params)
          :SensorNoise (:SensorNoise params)
          :BeliefNoise (:BeliefNoise params)}))

(defn proceed-n-steps
  [n steps time truedata or-state]
  (loop [t time
         ors or-state]
    (if (or (> t steps) (= t (+ n time))) ors
        (recur (inc t) (update-in ors [:sensors] update-sensors (get truedata t) t)))))

(defn hypothesize
  [problem or-state time-now params]
  (update-one-run-state or-state
   ((:hypothesize-fn problem) (:ep-state or-state) (:sensors or-state) time-now params)))

(defn run-simulation-step
  [problem truedata or-state params monitor? player?]
  (let [time (:time (:ep-state or-state))
        ors-sensors (proceed-n-steps (:StepsBetween params) (:Steps params)
                                     time truedata or-state)
        time-now (min (:Steps params) (+ (dec (:StepsBetween params)) time))
        ;; start the clock
        start-time (. System (nanoTime))
        ors-hyps (hypothesize problem ors-sensors time-now params)
        ep-explained (explain (:ep-state ors-hyps) (:consistent?-fn problem))
        ors-expl (proceed-one-run-state ors-hyps ep-explained time-now problem)
        ;; perform meta-abduction if the :bad set is non-empty
        ;; and meta-abduction is turned 'on'
        ors-meta
        (let [bad (:bad (:problem-data (current-ep-state (:ep-state-tree ors-expl))))]
          (if (or (empty? bad) (not (:meta-abduction ors-expl)))
            ors-expl
            (explain-meta problem ors-expl bad params)))
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-resources (update-in ors-meta [:resources] assoc :milliseconds ms)
        ors-results (if-not (or player? monitor?) ors-resources
                            (evaluate problem truedata ors-resources params))]
    (if (and (not player?) monitor?)
      ((:monitor-fn problem) problem truedata (:sensors ors-results) ors-results params)
      ors-results)))

(defn run-simulation
  [problem truedata or-state params monitor?]
  (loop [ors or-state]
    (when (nil? ors) (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= (:time (:ep-state ors)) (:Steps params))
      (last (:results (evaluate problem truedata ors params)))
      (recur (run-simulation-step problem truedata ors params monitor? false)))))

(defn run-comparative
  [problem monitor? meta? params]
  (let [truedata ((:truedata-fn problem) params)
        sensors ((:sensor-gen-fn problem) params)
        problem-data ((:gen-problem-data-fn problem) sensors params)
        or-states (init-one-run-states
                   {:MetaAbduction (if meta? [true false] [false]) :Lazy [true]}
                   sensors problem-data)]
    (doall (for [ors or-states]
             (binding [last-id 0]
               (run-simulation problem truedata ors params monitor?))))))

(defn run-many
  [problem monitor? meta? params repetitions]
  (doall
   (for [i (range repetitions)]
     (let [runs (run-comparative problem monitor? meta? params)]
       (if meta?
         ;; if we have meta vs. non-meta, evaluate the 'batch' form
         (evaluate-comparative problem params runs)
         ;; otherwise, just keep the original results from the single (non-meta) run
         (first runs))))))

(defn get-headers
  [problem]
  (concat (:headers problem)
          [:Step
           :Steps
           :StepsBetween
           :SensorNoise
           :BeliefNoise
           :MetaAbduction
           :Lazy
           :MetaAbductions
           :MetaAcceptedBad
           :MetaAcceptedImpossible
           :MetaAcceptedImpossibleLconf
           :MetaAcceptedNone
           :Milliseconds
           :Unexplained
           :ExplainCycles
           :HypothesisCount
           :HypothesesNew]))

(defn get-comparative-headers
  [problem]
  (concat (:comparative-headers problem)
          [:MetaAbductions
           :MetaAcceptedBad
           :MetaAcceptedImpossible
           :MetaAcceptedImpossibleLconf
           :MetaAcceptedNone
           :MetaMilliseconds
           :BaseMilliseconds
           :RatioMilliseconds
           :IncreaseMilliseconds
           :MetaUnexplained
           :BaseUnexplained
           :RatioUnexplained
           :MetaExplainCycles
           :BaseExplainCycles
           :RatioExplainCycles
           :IncreaseExplainCycles
           :Steps
           :StepsBetween
           :SensorNoise
           :BeliefNoise]))

(defrecord Problem
    [name headers comparative-headers monitor-fn player-fns
     truedata-fn sensor-gen-fn prepared-map
     hypothesize-fn commit-decision-fn gen-problem-data-fn
     consistent?-fn evaluate-fn evaluate-comparative-fn])
