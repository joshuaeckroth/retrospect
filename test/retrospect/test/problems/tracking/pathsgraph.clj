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
          h1 (new-hyp "Mov" :movement nil 1.0 :and [st1 sf2] "d1->d2"
                      {:det det1 :det2 det2})
          h2 (new-hyp "Mov" :movement nil 1.0 :and [st2 sf3] "d2->d3"
                      {:det det2 :det2 det3})
          entities {(symbol "A") [{:x 0 :y 0 :time 0 :color red}]}
          pg (build-paths-graph [h1 h2] entities)
          paths (paths-graph-paths pg)]
      (is (= #{det1 (assoc det2 :color red) det3} (nodes pg)))
      (is (= [[(assoc-in h1 [:data :det2] (assoc det2 :color red))
               (assoc-in h2 [:data :det] (assoc det2 :color red))]]
             (vec (map vec paths)))))))

(deftest bad-edges-1
  (binding [last-id 0]
    (let [det1 {:x 0 :y 0 :time 0 :color red}
          det2 {:x 1 :y 1 :time 1 :color gray}
          det3 {:x 2 :y 2 :time 2 :color blue}
          st1 (new-hyp "SensTo" :sensor-to nil 1.0 nil [] "" {:det det1})
          sf2 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det2})
          st2 (new-hyp "SensTo" :sensor-to nil 1.0 nil [] "" {:det det2})
          sf3 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det3})
          h1 (new-hyp "Mov" :movement nil 1.0 :and [st1 sf2] "d1->d2"
                      {:det det1 :det2 det2})
          h2 (new-hyp "Mov" :movement nil 1.0 :and [st2 sf3] "d2->d3"
                      {:det det2 :det2 det3})
          entities {(symbol "A") [{:x 0 :y 0 :time 0 :color red}]}
          pg (build-paths-graph [h1 h2] entities)
          paths (paths-graph-paths pg)]
      (is (= #{det1 (assoc det2 :color red) det3} (nodes pg)))
      (is (= [[(assoc-in h1 [:data :det2 :color] red)]] (vec (map vec paths)))))))

(deftest bad-edges-2
  (binding [last-id 0]
    (let [det1 {:x 0 :y 0 :time 0 :color red}
          det2 {:x 1 :y 1 :time 1 :color gray} ;; should turn red
          det3 {:x 0 :y 2 :time 2 :color blue} ;; should be part of a "bad edge"
          det4 {:x 2 :y 2 :time 2 :color red}
          det5 {:x 1 :y 2 :time 2 :color gray} ;; should turn red
          st1 (new-hyp "SensTo" :sensor-to nil 1.0 nil [] "" {:det det1})
          sf2 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det2})
          st2 (new-hyp "SensTo" :sensor-to nil 1.0 nil [] "" {:det det2})
          sf3 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det3})
          sf4 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det4})
          sf5 (new-hyp "SensFrom" :sensor-from nil 1.0 nil [] "" {:det det5})
          h1 (new-hyp "Mov" :movement nil 1.0 :and [st1 sf2] "d1->d2"
                      {:det det1 :det2 det2})
          h2 (new-hyp "Mov" :movement nil 1.0 :and [st2 sf3] "d2->d3"
                      {:det det2 :det2 det3})
          h3 (new-hyp "Mov" :movement nil 1.0 :and [st2 sf4] "d2->d4"
                      {:det det2 :det2 det4})
          h4 (new-hyp "Mov" :movement nil 1.0 :and [st2 sf5] "d2->d5"
                      {:det det2 :det2 det5})
          entities {(symbol "A") [{:x 0 :y 0 :time 0 :color red}]}
          pg (build-paths-graph [h1 h2 h3 h4] entities)
          paths (paths-graph-paths pg)]
      (is (= #{det1 (assoc det2 :color red) det3 det4 (assoc det5 :color red)}
             (nodes pg)))
      (is (= [[(assoc-in h1 [:data :det2 :color] red)
               (-> h4 (assoc-in [:data :det :color] red)
                   (assoc-in [:data :det2 :color] red))]
              [(assoc-in h1 [:data :det2 :color] red)
               (assoc-in h3 [:data :det :color] red)]]
             (vec (map vec paths)))))))

