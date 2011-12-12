(ns retrospect.test.problems.tracking.pathsgraph
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.colors])
  (:use [retrospect.workspaces :only [last-id new-hyp]])
  (:use [loom.graph :only [nodes edges incoming neighbors]])
  (:use [loom.attr :only [add-attr attr]])
  (:use [retrospect.problems.tracking.pathsgraph]))

(deftest color-update
  (binding [last-id 0]
    (let [det1 {:x 0 :y 0 :time 0 :color red}
          det2 {:x 1 :y 1 :time 1 :color gray}
          det3 {:x 2 :y 2 :time 2 :color red}
          st1 (new-hyp "SensTo" :sensor-to nil 1.0 nil [] "" {:det det1})
          sf2 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det2})
          st2 (new-hyp "SensTo" :sensor-to nil 1.0 nil [] "" {:det det2})
          sf3 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det3})
          h1 (new-hyp "Mov" :movement nil 1.0 :and [st1 sf2] "" {:det det1 :det2 det2})
          h2 (new-hyp "Mov" :movement nil 1.0 :and [st2 sf3] "" {:det det2 :det2 det3})
          entities {(symbol "A") [{:x 0 :y 0 :time 0 :color red}]}
          pg (build-paths-graph [h1 h2] entities)
          paths (paths-graph-paths pg)]
      (is (= #{det1 (assoc det2 :color red) det3} (nodes pg)))
      (is (= [[(assoc-in h1 [:data :det2] (assoc det2 :color red))
               (assoc-in h2 [:data :det] (assoc det2 :color red))]]
             (vec (map vec paths)))))))