(ns retrospect.problem
  (:import (java.util.concurrent ExecutionException))
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.onerun :only
         [init-one-run-states init-one-run-state
          update-one-run-state proceed-one-run-state]])
  (:use [retrospect.epistemicstates :only
         [explain previous-ep-state current-ep-state]])
  (:use [retrospect.meta.reason :only
         [meta-strategies metareasoning-activated? metareason]])
  (:use [retrospect.sensors :only [update-sensors]])
  (:use [retrospect.random :only [set-seed my-rand-int]]))

(defn avg-with-prior
  [results key val]
  (let [c (count results)]
    (if (= c 0) val
        (double (/ (+ (* c (key (last results))) val) (inc c))))))

(defn add-to-prior
  [results key val]
  (let [c (count results)]
    (if (= c 0) val
        (+ (key (last results)) val))))

(defn evaluate
  [problem truedata or-state or-state-meta params]
  (let [prev-ep (previous-ep-state (:ep-state-tree or-state))
        prev-ep-meta (previous-ep-state (:ep-state-tree or-state-meta))
        ep-state (current-ep-state (:ep-state-tree or-state))
        ep-state-meta (current-ep-state (:ep-state-tree or-state-meta))
        results (:results or-state-meta)
        res (:resources or-state-meta)]
    (update-in
     or-state-meta [:results] conj
     (merge ((:evaluate-fn problem) ep-state-meta results prev-ep-meta
             (:sensors or-state-meta) truedata params)
            ((:evaluate-meta-fn problem) ep-state ep-state-meta
             results truedata params)
            (assoc params
              :Step (:time ep-state-meta)
              :MetaStrategy (name (:meta-strategy or-state-meta)) 
              :Lazy (:lazy or-state-meta)
              :MetaActivations (:meta-activations res)
              :Milliseconds (:milliseconds res)
              :BadCount (add-to-prior results :BadCount
                                      (count (:bad (:problem-data ep-state-meta))))
              :Unexplained
              (add-to-prior
               results :Unexplained
               (count (:unexplained (:final (:log (:workspace prev-ep-meta))))))
              :SharedExplainsCount
              (add-to-prior
               results :SharedExplainsCount
               (count (:shared-explains (:final (:log (:workspace prev-ep-meta))))))
              ;; ExplainCycles are stored in current ep-state
              :ExplainCycles
              (:explain-cycles (:resources (:workspace ep-state-meta)))
              :HypothesisCount
              (:hyp-count (:resources (:workspace prev-ep-meta))))))))

(defn calc-percent-increase
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (* 100.0 (/ (- (k m) (k b)) (k b))))))

(defn calc-ratio
  [k m b]
  (if (= 0 (k b)) 0.0
      (double (/ (k m) (k b)))))

(defn evaluate-comparative
  [problem params m b]
  (merge ((:evaluate-comparative-fn problem) params m b)
         {:MetaStrategy (name (:MetaStrategy m)) 
          :MetaActivations (:MetaActivations m)
          :MetaMilliseconds (:Milliseconds m)
          :BaseMilliseconds (:Milliseconds b)
          :RatioMilliseconds (calc-ratio :Milliseconds m b)
          :IncreaseMilliseconds (calc-percent-increase :Milliseconds m b)
          :MetaBadCount (:BadCount m)
          :BaseBadCount (:BadCount b)
          :RatioBadCount (calc-ratio :BadCount m b)
          :IncreaseBadCount (calc-percent-increase :BadCount m b)
          :MetaSharedExplainsCount (:SharedExplainsCount m)
          :BaseSharedExplainsCount (:SharedExplainsCount b)
          :RatioSharedExplainsCount (calc-ratio :SharedExplainsCount m b)
          :IncreaseSharedExplainsCount (calc-percent-increase
                                         :SharedExplainsCount m b)
          :MetaUnexplained (:Unexplained m)
          :BaseUnexplained (:Unexplained b)
          :RatioUnexplained (calc-ratio :Unexplained m b)
          :IncreaseUnexplained (calc-percent-increase :Unexplained m b)
          :MetaExplainCycles (:ExplainCycles m)
          :BaseExplainCycles (:ExplainCycles b)
          :RatioExplainCycles (calc-ratio :ExplainCycles m b)
          :IncreaseExplainCycles (calc-percent-increase :ExplainCycles m b)
          :Steps (:Steps params)
          :StepsBetween (:StepsBetween params)
          :SensorNoise (:SensorNoise params)
          :BeliefNoise (:BeliefNoise params)
          :Seed (:Seed m)}))

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
        ep-state (:ep-state ors-hyps)
        ep-explained (explain ep-state (:get-more-hyps-fn problem)
                              (:inconsistent-fn problem))
        ors-expl (proceed-one-run-state ors-hyps ep-explained time-now problem)
        ors-meta (if (metareasoning-activated? ors-expl params)
                   (metareason ors-expl params) ors-expl)
        ;; stop the clock
        ms (/ (- (. System (nanoTime)) start-time) 1000000.0)
        ors-resources (update-in ors-meta [:resources] assoc :milliseconds ms)
        ors-results (evaluate problem truedata ors-expl ors-resources params)]
    (if (and (not player?) monitor?)
      ((:monitor-fn problem) problem truedata (:sensors ors-results)
       ors-results params)
      ors-results)))

(defn run-simulation
  [problem truedata or-state params monitor?]
  (loop [ors or-state]
    (when (nil? ors)
      (throw (ExecutionException. "Monitor took control." (Throwable.))))
    (if (>= (:time (:ep-state ors)) (:Steps params))
      (last (:results ors))
      (recur (run-simulation-step problem truedata ors params monitor? false)))))

(defn run
  [problem monitor? datadir params]
  (println "Running" params)
  (set-seed (:Seed params))
  (let [truedata ((:truedata-fn problem) datadir params)
        sensors ((:sensor-gen-fn problem) params)
        problem-data ((:gen-problem-data-fn problem) sensors datadir params)
        nometa-or-state (init-one-run-state :NoMetareasoning true sensors problem-data)
        meta-or-states (init-one-run-states {:MetaStrategy meta-strategies :Lazy [true]}
                                            sensors problem-data)
        nometa-result (binding [last-id 0]
                        (run-simulation problem truedata nometa-or-state params monitor?)) 
        meta-results (doall (for [ors meta-or-states]
                              (binding [last-id 0]
                                (run-simulation problem truedata ors params monitor?))))]
    (for [m meta-results]
      (evaluate-comparative problem params m nometa-result))))

(defn get-headers
  [problem]
  (concat (:headers problem)
          [:Step
           :Steps
           :StepsBetween
           :SensorNoise
           :BeliefNoise
           :MetaStrategy
           :Lazy
           :MetaActivations
           :Milliseconds
           :BadCount
           :SharedExplainsCount
           :Unexplained
           :ExplainCycles
           :HypothesisCount
           :Seed]))

(defn get-comparative-headers
  [problem]
  (concat (:comparative-headers problem)
          [:MetaStrategy
           :MetaActivations
           :MetaMilliseconds
           :BaseMilliseconds
           :RatioMilliseconds
           :IncreaseMilliseconds
           :MetaBadCount
           :BaseBadCount
           :RatioBadCount
           :IncreaseBadCount
           :MetaSharedExplainsCount
           :BaseSharedExplainsCount
           :RatioSharedExplainsCount
           :IncreaseSharedExplainsCount
           :MetaUnexplained
           :BaseUnexplained
           :RatioUnexplained
           :IncreaseUnexplained
           :MetaExplainCycles
           :BaseExplainCycles
           :RatioExplainCycles
           :IncreaseExplainCycles
           :Steps
           :StepsBetween
           :SensorNoise
           :BeliefNoise
           :Seed]))

(defrecord Problem
  [name headers meta-headers comparative-headers monitor-fn player-fns
   truedata-fn sensor-gen-fn prepared-map
   hypothesize-fn get-more-hyps-fn commit-decision-fn
   gen-problem-data-fn inconsistent-fn
   evaluate-fn evaluate-meta-fn evaluate-comparative-fn])
