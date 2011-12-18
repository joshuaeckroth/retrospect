(ns retrospect.problems.causal.prepared
  (:use [loom.graph :only [digraph add-nodes add-edges]])
  (:use [loom.attr :only [add-attr]])
  (:use [retrospect.problems.causal.sensors :only
         [generate-sensors]])
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
                      (add-edges ['A 'C])
                      (add-edges ['B 'C])
                      (add-attr 'A :apriori 0.5)
                      (add-attr 'B :apriori 0.5)
                      (add-attr 'A :value :off)
                      (add-attr 'B :value :on))]
      {:params (assoc basic-params :Steps 1 :StepsBetween 1)
       :sensors (generate-sensors)
       :truedata {:network network :observed-seq [[] ['C :off]]}})))

(defn simple-medium
  []
  (binding [rgen (new-seed 10)]
    (let [network (-> (digraph)
                      (add-edges ['A 'D])
                      (add-edges ['B 'D])
                      (add-edges ['C 'E])
                      (add-edges ['D 'F])
                      (add-edges ['D 'G])
                      (add-edges ['E 'G])
                      (add-edges ['X 'A])
                      (add-attr 'A :apriori 0.5)
                      (add-attr 'B :apriori 0.5)
                      (add-attr 'C :apriori 0.5)
                      (add-attr 'D :apriori 0.5)
                      (add-attr 'E :apriori 0.5)
                      (add-attr 'X :apriori 0.5)
                      (add-attr 'F :value :on)
                      (add-attr 'G :value :on))]
      {:params (assoc basic-params :Steps 2 :StepsBetween 1)
       :sensors (generate-sensors)
       :truedata {:network network :observed-seq [[] ['F :on] ['G :on]]}})))

(def prepared-map
  (sorted-map "abc" abc
              "simple-medium" simple-medium))
