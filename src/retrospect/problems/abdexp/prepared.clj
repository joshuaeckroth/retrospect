(ns retrospect.problems.abdexp.prepared
  (:use [loom.graph :only [digraph add-edges]])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [retrospect.problems.abdexp.sensors :only [generate-sensors]]))

(defn peyer
  []
  (let [expgraph (-> (digraph)
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
                    (set-conflicts ["G7" "I7"])
                    (fill "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8"
                          "E9" "E10" "E11" "E12" "E13" "E14" "E16" "E17"))]
    {:params {:Steps 1}
     :truedata {:training {:test {1 expgraph}}
                :test {1 expgraph}}
     :sensors (generate-sensors)}))

(def prepared-map
  (sorted-map "peyer" peyer))
