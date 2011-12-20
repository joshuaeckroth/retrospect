(ns retrospect.problems.causal.prepared
  (:use [loom.graph :only [digraph add-nodes add-edges]])
  (:use [loom.attr :only [add-attr]])
  (:use [retrospect.problems.causal.sensors :only
         [generate-sensors]])
  (:use [retrospect.problems.causal.javabayes :only
         [build-network build-bayesnet]])
  (:use [retrospect.random]))

(def basic-params
  {:Seed 10
   :Steps 25
   :Threshold 0
   :StepsBetween 6
   :SensorNoise 0
   :BeliefNoise 0
   :ObservableNodes 10
   :InternalNodes 10
   :MetaReasoning "NoMetareasoning"
   :TransitiveExplanation true})

(defn abc
  []
  (binding [rgen (new-seed 10)]
    (let [network (-> (digraph)
                      (add-edges ["A" "C"])
                      (add-edges ["B" "C"])
                      (add-attr "A" :probs [0.5 0.5])
                      (add-attr "B" :probs [0.5 0.5])
                      (add-attr "A" :values ["on" "off"])
                      (add-attr "B" :values ["on" "off"])
                      (add-attr "A" :value "off")
                      (add-attr "B" :value "on")
                      ;; CAB:               TTT TTF TFT TFF  FTT FTF FFT FFF
                      (add-attr "C" :probs [1.0 1.0 1.0 0.25 0.0 0.0 0.0 0.75])
                      (add-attr "C" :values ["on" "off"]))]
      {:params (assoc basic-params :Steps 1 :StepsBetween 1)
       :sensors (generate-sensors)
       :truedata {:network network
                  :bayesnet (build-bayesnet network)
                  :observed-seq [[] ["C" "on"]]}})))

(defn simple-medium
  []
  (binding [rgen (new-seed 10)]
    (let [network (-> (digraph)
                      (add-edges ["A" "D"])
                      (add-edges ["B" "D"])
                      (add-edges ["C" "E"])
                      (add-edges ["D" "F"])
                      (add-edges ["D" "G"])
                      (add-edges ["E" "G"])
                      (add-edges ["X" "A"])
                      ;; AX:               TT  TF  FT  FF
                      (add-attr "A" :probs [1.0 0.5 0.0 0.5])
                      (add-attr "A" :value "off")
                      (add-attr "A" :values ["on" "off"])
                      ;; B:                T   F
                      (add-attr "B" :probs [0.5 0.5])
                      (add-attr "B" :value "off")
                      (add-attr "B" :values ["on" "off"])
                      (add-attr "C" :probs [0.25 0.75])
                      (add-attr "C" :value "on")
                      (add-attr "C" :values ["on" "off"])
                      ;; DAB:              TTT TTF TFT TFF FTT FTF FFT FFF
                      (add-attr "D" :probs [1.0 1.0 1.0 0.5 0.0 0.0 0.0 0.5])
                      (add-attr "D" :value "off")
                      (add-attr "D" :values ["on" "off"])
                      ;; EC:               TT  TF  FT  FF
                      (add-attr "E" :probs [1.0 0.5 0.0 0.5])
                      (add-attr "E" :value "on")
                      (add-attr "E" :values ["on" "off"])
                      (add-attr "X" :probs [0.5 0.5])
                      (add-attr "X" :value "off")
                      (add-attr "X" :values ["on" "off"])
                      (add-attr "F" :value "off")
                      ;; FD:               TT  TF  FT  FF
                      (add-attr "F" :probs [1.0 0.5 0.0 0.5])
                      (add-attr "F" :values ["on" "off"])
                      (add-attr "G" :value "on")
                      ;; GDE:              TTT TTF TFT TFF FTT FTF FFT FFF
                      (add-attr "G" :probs [1.0 1.0 1.0 0.5 0.0 0.0 0.0 0.5])
                      (add-attr "G" :values ["on" "off"]))]
      {:params (assoc basic-params :Steps 2 :StepsBetween 1)
       :sensors (generate-sensors)
       :truedata {:network network
                  :bayesnet (build-bayesnet network)
                  :observed-seq [[] ["G" "on"] ["F" "off"]]}})))

(defn alarm
  []
  (let [bayesnet (Alarm.)]
    {:params basic-params
     :sensors (generate-sensors)
     :truedata {:network (build-network bayesnet)
                :bayesnet bayesnet
                :observed-seq [[]]}}))

(defn dog-problem
  []
  (let [bayesnet (DogProblem.)]
    {:params basic-params
     :sensors (generate-sensors)
     :truedata {:network (build-network bayesnet)
                :bayesnet bayesnet
                :observed-seq [[]]}}))

(def prepared-map
  (sorted-map "abc" abc
              "alarm" alarm
              "dog-problem" dog-problem
              "simple-medium" simple-medium))
