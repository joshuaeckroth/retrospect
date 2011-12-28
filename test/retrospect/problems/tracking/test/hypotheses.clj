(ns retrospect.problems.tracking.test.hypotheses
  (:use [clojure.test :only [deftest is]])
  (:use [retrospect.colors])
  (:use [retrospect.workspaces :only [last-id new-hyp]])
  (:use [retrospect.problems.tracking.hypotheses])
  (:use [retrospect.problems.tracking.pathsgraph :only
         [build-paths-graph paths-graph-paths]])
  (:use [retrospect.state]))

(deftest gaps
  (binding [last-id 0
            compute 0
            memory 0
            params {:PathBranches 2 :GridWidth 10 :GridHeight 10}]
    (let [det1 {:x 0 :y 0 :color red :time 0}
          det2 {:x 0 :y 1 :color red :time 1}
          det3 {:x 0 :y 2 :color gray :time 2}
          det4 {:x 0 :y 3 :color gray :time 3}
          st1 (new-hyp "SensTo" :sensor :sensor-to nil 1.0 nil [] [] "" {:det det1})
          sf2 (new-hyp "SensFrom" :sensor :sensor-from nil 1.0 nil [] [] "" {:det det2})
          st2 (new-hyp "SensTo" :sensor :sensor-to nil 1.0 nil [] [] "" {:det det2})
          sf3 (new-hyp "SensFrom" :sensor :sensor-from nil 1.0 nil [] [] "" {:det det3})
          st3 (new-hyp "SensTo" :sensor :sensor-from nil 1.0 nil [] [] "" {:det det3})
          sf4 (new-hyp "SensFrom" :sensor :sensor-from nil 1.0 nil [] [] "" {:det det4})
          h1 (new-hyp "Mov" :movement :movement nil 1.0 :and [st1 sf2] [st1 sf2] "d1->d2"
                      {:det det1 :det2 det2})
          h3 (new-hyp "Mov" :movement :movement nil 1.0 :and [st3 sf4] [st1 sf2] "d3->d4"
                      {:det det3 :det2 det4})
          h3-color (-> h3 (assoc-in [:data :det :color] red)
                       (assoc-in [:data :det2 :color] red))
          uncovered-from #{sf3}
          uncovered-to #{st2}
          walk-dist (with-meta {1.0 1} {:walk-count 1})
          mov-hyps (make-movement-hyps uncovered-from uncovered-to walk-dist)
          mov-hyp-color (-> (first mov-hyps)
                            (assoc-in [:data :det :color] red)
                            (assoc-in [:data :det2 :color] red))
          entities {(symbol "A") [{:x 0 :y 1 :color red :time 1}]}
          pg (build-paths-graph (conj mov-hyps h1 h3) entities)
          paths (paths-graph-paths pg entities {})]
      (is (= 1 (count mov-hyps)))
      (is (= {:det det2 :det2 det3}
             (select-keys (:data (first mov-hyps)) [:det :det2])))
      (is (= {:straight [[mov-hyp-color h3-color]]
              :left [] :right []}
             paths)))))