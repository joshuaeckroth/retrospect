(ns retrospect.problems.abdexp.prepared
  (:use [loom.graph :only [digraph add-edges nodes]])
  (:use [loom.attr :only [add-attr attr]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.bayesnet])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]])
  (:use [granary.random]))

(defn bn-abc
  []
  (binding [rgen (new-seed 10)]
    (let [expgraph (-> (digraph)
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
                      (add-attr "C" :values ["on" "off"]))
          bn (build-bayesnet expgraph)]
      {:params {:Steps 1 :StepsBetween 1}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn}
                  :expgraph expgraph
                  :bayesnet bn
                  :true-values-map {}
                  :false-values-map {}
                  :test [[] ["C" "on"]]}})))

(defn bn-simple-medium
  []
  (let [expgraph (-> (digraph)
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
                    (add-attr "G" :values ["on" "off"]))
        bn (build-bayesnet expgraph)]
    {:params {:Steps 2 :StepsBetween 1}
     :sensors (generate-sensors {})
     :truedata {:training {:expgraph expgraph :bayesnet bn}
                :expgraph expgraph
                :bayesnet bn
                :true-values-map {}
                :false-values-map {}
                :test [[] ["G" "on"] ["F" "off"]]}}))

(comment
  (defn bn-alarm
    []
    (let [bn (load-bayesnet "ALARM.BIF")
          expgraph (build-expgraph bn)
          obs-seq [[] [["Intubation" "Normal"]]]]
      {:params {:Steps 1}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn}
                  :expgraph expgraph
                  :bayesnet bn
                  :true-values-map {}
                  :false-values-map {}
                  :test obs-seq}}))

  (defn bn-john-mary-call
    []
    (let [bn (load-bayesnet "john-mary-call.bif")
          expgraph (build-expgraph bn)
          obs-seq [[] [["MaryCalls" "True"]]]]
      {:params {:Steps 1}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn}
                  :expgraph expgraph
                  :bayesnet bn
                  :true-values-map {}
                  :false-values-map {}
                  :test obs-seq}}))

  (defn bn-car-starts
    []
    (let [bn (load-bayesnet "car-starts.bif")
          expgraph (build-expgraph bn)
          obs-seq [[] [["Starts" "No"]] [["Leak" "NoLeak"]]]]
      {:params {:Steps 2}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn}
                  :expgraph expgraph
                  :bayesnet bn
                  :true-values-map {}
                  :false-values-map {}
                  :test obs-seq}}))

  (defn bn-cancer
    []
    (let [bn (load-bayesnet "CANCER.BIF")
          expgraph (build-expgraph bn)
          obs-seq [[] [["SevereHeadaches" "Present"]]]]
      {:params {:Steps 1}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn}
                  :expgraph expgraph
                  :bayesnet bn
                  :true-values-map {}
                  :false-values-map {}
                  :test obs-seq}}))

  (defn bn-hail-finder
    []
    (let [bn (load-bayesnet "hailfinder25.bif")
          expgraph (build-expgraph bn)
          obs-seq [[] [["PlainsFcst" "SVR"]]]]
      {:params {:Step 1}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn}
                  :expgraph expgraph
                  :bayesnet bn
                  :true-values-map {}
                  :false-values-map {}
                  :test obs-seq}})))

(def prepared-map
  (sorted-map "bn-abc" bn-abc
              "bn-simple-medium" bn-simple-medium))

(comment
  "bn-alarm" bn-alarm
  "bn-cancer" bn-cancer
  "bn-car-starts" bn-car-starts
  "bn-hail-finder" bn-hail-finder
  "bn-john-mary-call" bn-john-mary-call)
