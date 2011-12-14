(ns retrospect.test.workspaces
  (:use [clojure.test :only [deftest is use-fixtures]])
  (:use [clojure.contrib.seq :only [find-first]])
  (:use [retrospect.test.utils])
  (:use [retrospect.workspaces]))

(def workspace nil)
(def s1 nil)
(def s2 nil)
(def a nil)
(def b nil)
(def c nil)
(def d nil)

(defn simple-hyps-fixture [f]
  "s1, s2 are facts; a->s1,s2, b->s2, c->a, d->a,b (explains arrows)."
  (binding [last-id 0]
    (let [s1 (new-hyp "S" :sensor (constantly []) 1.0 nil [] [] "" {})
          s2 (new-hyp "S" :sensor (constantly []) 1.0 nil [] [] "" {})
          a (new-hyp "A" :hyp (constantly []) 0.75 :and [s1 s2] [s1 s2] "" {})
          b (new-hyp "B" :hyp (constantly []) 0.5  :and [s2] [s2] "" {})
          c (new-hyp "C" :hyp (constantly []) 0.25 :and [a] [a] "" {})
          d (new-hyp "D" :hyp (constantly []) 0.6  :and [a b] [a b] "" {})
          workspace (-> (init-workspace)
                        (add s1 :static) (force-accept s1)
                        (add s2 :static) (force-accept s2)
                        (add a :static)
                        (add b :static)
                        (add c :static)
                        (add d :static)
                        (prepare-workspace))]
      (binding [workspace workspace
                s1 s1 s2 s2 a a b b c c d d]
        (f)))))

(use-fixtures :each simple-hyps-fixture)

(deftest explainers-nontrans
  (let [expl (find-all-explainers workspace false)
        ws (update-confidences workspace expl)
        expl-sorted (sort-explainers ws expl)]
    (is (= #{a} (set (:explainers (find-first #(= (:hyp %) s1) expl)))))
    (is (= #{a b} (set (:explainers (find-first #(= (:hyp %) s2) expl)))))
    (is (nil? (:explainers (find-first #(= (:hyp %) a) expl))))
    (is (nil? (:explainers (find-first #(= (:hyp %) b) expl))))
    (is (approx= 0.75 (hyp-conf workspace a) 0.001))
    (is (approx= 0.5 (hyp-conf workspace b) 0.001))
    ;; order matters in the expl sequence, so let's make sure it is consistent
    (is (= [s1 s2] (map :hyp expl)))
    (is (not-empty expl-sorted))
    (is (= [s1 s2] (map :hyp expl-sorted)))
    (is (= [a] (:explainers (first expl-sorted))))
    (is (approx= 0.7375 (hyp-conf ws a) 0.001))
    (is (approx= 0.45 (hyp-conf ws b) 0.001))))

(deftest explainers-trans
  (let [expl (find-all-explainers workspace true)
        ws (update-confidences workspace expl)
        expl-sorted (sort-explainers ws expl)]
    (is (= #{a c d} (set (:explainers (find-first #(= (:hyp %) s1) expl)))))
    (is (= #{a b c d} (set (:explainers (find-first #(= (:hyp %) s2) expl)))))
    ;; order matters in the expl sequence, so let's make sure it is consistent
    (is (= [s1 s2] (map :hyp expl)))
    (is (not-empty expl-sorted))
    ;; s2 has a better explainer than s1
    (is (= [s2 s1] (map :hyp expl-sorted)))
    (is (= [a d b c] (:explainers (first expl-sorted))))
    (is (= [a d c] (:explainers (second expl-sorted))))))

(deftest best-nontrans
  (let [expl (find-all-explainers workspace false)
        ws (update-confidences workspace expl)
        expl-sorted (sort-explainers ws expl)]
    (is (= {:best a :alts #{} :essential? true :delta nil :explained s1}
           (find-best ws expl-sorted 0)))
    (is (= [{:hyp s2 :explainers [a b]}] (vec (filter #(not= s1 (:hyp %)) expl-sorted))))
    (is (= {:best a :alts [b] :essential? false :explained s2}
           (select-keys (find-best ws (filter #(not= s1 (:hyp %)) expl-sorted) 0)
                        [:best :alts :essential? :explained])))
    (is (approx= (- 0.7375 0.45)
                 (:delta (find-best ws (filter #(not= s1 (:hyp %)) expl-sorted) 0))
                 0.001))))
