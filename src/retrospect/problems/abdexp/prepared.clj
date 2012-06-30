(ns retrospect.problems.abdexp.prepared
  (:use [loom.graph :only [digraph add-edges]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]]))

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
  (let [expgraph-1 (force-fill (peyer-setup) "E1" "E2" "E3" "E4" "E5")
        expgraph-2 (force-fill expgraph-1 "E6" "E7" "E8" "E9" "E10" "E11")
        expgraph-3 (force-fill expgraph-2 "E12" "E13" "E14" "E16" "E17")
        progression {1 expgraph-1, 2 expgraph-2, 3 expgraph-3}]
    {:params {:Steps 3}
     :truedata {:training {:test progression}
                :test progression
                :true-vertices #{"E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "E10"
                                 "E11" "E12" "E13" "E14" "E16" "E17"}}
     :sensors (generate-sensors)}))

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
  (let [expgraph-1 (force-fill (hr23-setup)
                               "accident" "tire marks"
                               "occupants reported arguing")
        expgraph-2 (-> expgraph-1
                      (add-edges ["passenger pulls handbrake" "wheels locked"]
                                 ["driver pulls handbrake" "wheels locked"]
                                 ["passenger pulls handbrake" "handbrake pulled"]
                                 ["driver pulls handbrake" "handbrake pulled"]
                                 ["passenger pulls handbrake" "driver says passenger pulled handbrake"]
                                 ["driver is lying" "driver says passenger pulled handbrake"])
                      (force-fill "handbrake pulled" "driver says passenger pulled handbrake")
                      (set-conflicts ["driver pulls handbrake" "passenger pulls handbrake"]))
        expgraph-3 (-> expgraph-2
                      (add-edges ["passenger drunk" "passenger pulls handbrake"])
                      (force-fill "passenger drunk"))
        progression {1 expgraph-1, 2 expgraph-2, 3 expgraph-3}]
    {:params {:Steps 3}
     :truedata {:training {:test progression}
                :test progression
                :true-vertices #{"passenger pulls handbrake"}}
     :sensors (generate-sensors)}))

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
  (let [expgraph-1 (force-fill (noexp-setup)
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
                :true-vertices #{"1" "2" "3" "4" "21" "30"}}
     :sensors (generate-sensors)}))

(def prepared-map
  (sorted-map "peyer" peyer
              "hr23" hr23
              "noexp" noexp))
