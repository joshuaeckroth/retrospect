(ns retrospect.problems.abdexp.prepared
  (:use [loom.graph :only [digraph add-edges nodes]])
  (:use [loom.attr :only [add-attr attr]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.javabayes])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]])
  (:use [retrospect.random]))

(defn peyer-setup
  []
  (-> (digraph)
     (add-edges ["I1" "E1"]
                ["G1" "E1"]
                ["G6" "E2"]
                ["G4" "E3"]
                ["G5" "E4"]
                ["I2" "E4"]
                ["I2" "E5"]
                ["G3" "E6"]
                ["I3" "E6"]
                ["G1" "E7"]
                ["I4" "E7"]
                ["I4a" "E8"]
                ["G2" "E9"]
                ["I5" "E9"]
                ["G1" "E10"]
                ["I6" "E10"]
                ["I6" "E11"]
                ["G7" "E12"]
                ["I7" "E12"]
                ["G8" "E13"]
                ["I7" "E13"]
                ["G1" "E14"]
                ["I1" "E14"]
                ;; ignore E15
                ["I1" "E16"]
                ["I8" "E17"]
                ["I4a" "I4"])
     (set-conflicts ["G1" "I1"])
     (set-conflicts ["G1" "I8"])
     (set-conflicts ["G2" "I5"])
     (set-conflicts ["G3" "I3"])
     (set-conflicts ["G5" "I2"])
     (set-conflicts ["G7" "I7"])))

(defn peyer
  []
  (let [expgraph-1 (force-on (peyer-setup) "E1" "E2" "E3" "E4" "E5")
        expgraph-2 (force-on expgraph-1 "E6" "E7" "E8" "E9" "E10" "E11")
        expgraph-3 (force-on expgraph-2 "E12" "E13" "E14" "E16" "E17")
        progression {1 expgraph-1, 2 expgraph-2, 3 expgraph-3}]
    {:params {:Steps 3}
     :truedata {:training {:test progression}
                :test progression
                :true-obs #{"E1" "E2" "E3" "E4" "E5"}
                :true-values-map {"E1" "on" "E2" "on" "E3" "on" "E4" "on"
                                  "E5" "on" "E6" "on" "E7" "on" "E8" "on"
                                  "E9" "on" "E10" "on" "E11" "on" "E12" "on"
                                  "E13" "on" "E14" "on" "E16" "on" "E17" "on"}}
     :sensors (generate-sensors {})}))

(defn hr23-setup
  []
  (-> (digraph)
     (add-edges ["skidding" "accident"]
                ["skidding" "tire marks"]
                ["loss of control" "skidding"]
                ["argument" "occupants reported arguing"]
                ["argument" "loss of control"]
                ["wheels locked" "skidding"]
                ["speeding in curve" "loss of control"])))

(defn hr23
  []
  (let [expgraph-1 (force-on (hr23-setup)
                               "accident" "tire marks"
                               "occupants reported arguing")
        expgraph-2 (-> expgraph-1
                      (add-edges ["passenger pulls handbrake" "wheels locked"]
                                 ["driver pulls handbrake" "wheels locked"]
                                 ["passenger pulls handbrake" "handbrake pulled"]
                                 ["driver pulls handbrake" "handbrake pulled"]
                                 ["passenger pulls handbrake" "driver says passenger pulled handbrake"]
                                 ["driver is lying" "driver says passenger pulled handbrake"])
                      (force-on "handbrake pulled" "driver says passenger pulled handbrake")
                      (set-conflicts ["driver pulls handbrake" "passenger pulls handbrake"]))
        expgraph-3 (-> expgraph-2
                      (add-edges ["passenger drunk" "passenger pulls handbrake"])
                      (force-on "passenger drunk"))
        progression {1 expgraph-1, 2 expgraph-2, 3 expgraph-3}]
    {:params {:Steps 3}
     :truedata {:training {:test progression}
                :test progression
                :true-obs #{"accident" "tire marks" "occupants reported arguing"}
                :true-values-map {"passenger pulls handbrake" "on"}}
     :sensors (generate-sensors {})}))

(defn noexp-setup
  []
  (-> (digraph)
     (add-edges ["10" "1"]
                ["11" "2"]
                ["12" "3"]
                ["13" "4"]
                ["20" "10"]
                ["21" "11"]
                ["22" "12"]
                ["23" "10"]
                ["23" "13"]
                ["30" "11"]
                ["30" "23"])
     (set-conflicts ["20" "21"])))

(defn noexp
  []
  (let [expgraph-1 (force-on (noexp-setup)
                               "1" "2" "3" "4")
        expgraph-2 (-> expgraph-1
                      (add-edges ["40" "31"]
                                 ["31" "23"]
                                 ["31" "22"]
                                 ["31" "23"])
                      (set-conflicts ["30" "31"] ["31" "20"]))
        progression {1 expgraph-1, 2 expgraph-2}]
    {:params {:Steps 2}
     :truedata {:training {:test progression}
                :test progression
                :true-obs #{"1" "2" "3" "4"}
                :true-explainers-map {"1" "on" "2" "on" "3" "on" "4" "on"
                                      "21" "on" "30" "on"}}
     :sensors (generate-sensors {})}))

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
                  :true-obs #{}
                  :true-values-map {}
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
                :true-obs #{}
                :true-values-map {}
                :test [[] ["G" "on"] ["F" "off"]]}}))

(defn bn-alarm
  []
  (binding [rgen (new-seed 10)]
    (let [bn (load-bayesnet "ALARM.BIF")
          expgraph (build-expgraph bn)
          obs-nodes (take 5 (my-shuffle (nodes expgraph)))
          obs-seq (for [n obs-nodes]
                    [n (my-rand-nth (attr expgraph n :values))])
          bn2 (observe-seq bn obs-seq)]
      {:params {:Steps 1}
       :sensors (generate-sensors {})
       :truedata {:training {:expgraph expgraph :bayesnet bn2}
                  :expgraph expgraph
                  :bayesnet bn2
                  :true-obs #{}
                  :true-values-map {}
                  :test (vec (concat [[]] obs-seq))}})))

(defn bn-john-mary-call
  []
  (let [bn (load-bayesnet "john-mary-call.bif")
        expgraph (build-expgraph bn)
        obs-seq [["MaryCalls" "True"]]
        bn2 (observe-seq bn obs-seq)]
    {:params {:Steps 1}
     :sensors (generate-sensors {})
     :truedata {:training {:expgraph expgraph :bayesnet bn2}
                :expgraph expgraph
                :bayesnet bn2
                :true-obs #{}
                :true-values-map {}
                :test (vec (concat [[]] obs-seq))}}))

(defn bn-car-starts
  []
  (let [bn (load-bayesnet "car-starts.bif")
        expgraph (build-expgraph bn)
        obs-seq [["Starts" "No"] ["Leak" "NoLeak"]]
        bn2 (observe-seq bn obs-seq)]
    {:params {:Steps 2}
     :sensors (generate-sensors {})
     :truedata {:training {:expgraph expgraph :bayesnet bn2}
                :expgraph expgraph
                :bayesnet bn2
                :true-obs #{}
                :true-values-map {}
                :test (vec (concat [[]] obs-seq))}}))

(defn bn-cancer
  []
  (let [bn (load-bayesnet "CANCER.BIF")
        expgraph (build-expgraph bn)
        obs-seq [["SevereHeadaches" "Present"]]
        bn2 (observe-seq bn obs-seq)]
    {:params {:Steps 1}
     :sensors (generate-sensors {})
     :truedata {:training {:expgraph expgraph :bayesnet bn2}
                :expgraph expgraph
                :bayesnet bn2
                :true-obs #{}
                :true-values-map {}
                :test (vec (concat [[]] obs-seq))}}))

(defn bn-hail-finder
  []
  (let [bn (load-bayesnet "hailfinder25.bif")
        expgraph (build-expgraph bn)
        obs-seq [["PlainsFcst" "SVR"]]
        bn2 (observe-seq bn obs-seq)]
    {:params {:Step 1}
     :sensors (generate-sensors {})
     :truedata {:training {:expgraph expgraph :bayesnet bn2}
                :expgraph expgraph
                :bayesnet bn2
                :true-obs #{}
                :true-values-map {}
                :test (vec (concat [[]] obs-seq))}}))

(def prepared-map
  (sorted-map "peyer" peyer
              "hr23" hr23
              "noexp" noexp
              "bn-abc" bn-abc
              "bn-alarm" bn-alarm
              "bn-cancer" bn-cancer
              "bn-car-starts" bn-car-starts
              "bn-hail-finder" bn-hail-finder
              "bn-john-mary-call" bn-john-mary-call
              "bn-simple-medium" bn-simple-medium))
