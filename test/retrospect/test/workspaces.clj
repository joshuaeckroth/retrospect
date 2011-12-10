(ns retrospect.test.workspaces
  (:use [clojure.test :only [deftest is use-fixtures]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.workspaces]))

(defn approx=
  "Return true if the absolute value of the difference between x and y
   is less than eps."
  [x y eps]
  (< (Math/abs (- x y)) eps))

(def workspace nil)
(def s1 nil)
(def s2 nil)
(def a nil)
(def b nil)

(defn simple-hyps-fixture [f]
  "s1, s2 are facts; a->s1, a->s2, b->s2 (explains arrows)."
  (binding [last-id 0]
    (let [s1 (new-hyp "S" :sensor (constantly []) 1.0 nil [] "" {})
          s2 (new-hyp "S" :sensor (constantly []) 1.0 nil [] "" {})
          a (new-hyp "A" :hyp (constantly []) 0.75 :and [s1 s2] "" {})
          b (new-hyp "B" :hyp (constantly []) 0.5  :and [s2] "" {})
          workspace (-> (init-workspace)
                        (add s1 :static) (force-accept s1)
                        (add s2 :static) (force-accept s2)
                        (add a :static)
                        (add b :static)
                        (prepare-workspace))]
      (binding [workspace workspace
                s1 s1 s2 s2 a a b b]
        (f)))))

(use-fixtures :each simple-hyps-fixture)

(deftest correct-explainers
  (let [expl (find-all-explainers workspace false)
        ws (update-confidences workspace expl)
        expl-sorted (sort-explainers ws expl)]
    (is (= #{a} (set (:explainers (find-first #(= (:hyp %) s1) expl)))))
    (is (= #{a b} (set (:explainers (find-first #(= (:hyp %) s2) expl)))))
    (is (= #{} (set (:explainers (find-first #(= (:hyp %) a) expl)))))
    (is (= #{} (set (:explainers (find-first #(= (:hyp %) b) expl)))))
    (is (= #{s1 s2} (set (map :hyp expl))))
    (is (approx= 0.75 (hyp-conf workspace a) 0.001))
    (is (approx= 0.5 (hyp-conf workspace b) 0.001))
    ;; order matters in the expl sequence, so let's make sure it's consistent
    (is (= s1 (:hyp (first expl))))
    (is (= s2 (:hyp (second expl))))
    (is (not-empty expl-sorted))
    (is (= s1 (:hyp (first expl-sorted))))
    (is (= #{a} (set (:explainers (first expl-sorted)))))
    (is (approx= 0.7375 (hyp-conf ws a) 0.001))
    (is (approx= 0.45 (hyp-conf ws b) 0.001))))


