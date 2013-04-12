(ns retrospect.problems.abdexp.expgraph-test
  (:use [clojure.test])
  (:use [retrospect.problems.abdexp.expgraph])
  (:use [loom.graph])
  (:use [loom.attr])
  (:use [retrospect.simulate :only [get-default-params]])
  (:use [retrospect.state]))

(deftest test-simple-expgraph
  (let [expgraph (-> (digraph)
                    (add-edges [:a :b])
                    (add-edges [:a :c])
                    (add-edges [:b :d])
                    (add-edges [:c :d])
                    (add-attr :a :values ["on" "off"])
                    (add-attr :b :values ["on" "off"])
                    (add-attr :c :values ["on" "off"])
                    (add-attr :d :values ["on" "off"])
                    (add-attr :a :probs {:table [0.3 0.7]
                                         :map {#{} {"on" 0.3 "off" 0.7}}})
                    (add-attr :b :probs {:table [0.4 0.6 0.2 0.8]
                                         :map {#{[:a "off"]} {"on" 0.4 "off" 0.6}
                                               #{[:a "on"]} {"on" 0.2 "off" 0.8}}}))]
    (is (vertex? expgraph :a))
    (is (vertex? expgraph :b))
    (is (vertex? expgraph :c))
    (is (vertex? expgraph :d))
    (is (= ["on" "off"] (values expgraph :a)))
    (is (= [[:a "off"] [:a "on"] [:b "off"] [:b "on"]
            [:c "off"] [:c "on"] [:d "off"] [:d "on"]]
           (vertex-value-pairs expgraph)))
    (is (= [[:a "off"] [:a "on"] [:c "off"] [:c "on"]]
           (vertex-value-pairs expgraph [:a :c])))
    (is (= {:table [0.3 0.7] :map {#{} {"on" 0.3 "off" 0.7}}} (probs expgraph :a)))
    (is (= 0.3 (binding [params {:PriorFunc "max"}]
                 (prob expgraph :a "on" #{}))))
    (is (= 0.3 (binding [params {:PriorFunc "min"}]
                 (prob expgraph :a "on" #{}))))
    (is (= 0.3 (binding [params {:PriorFunc "mavg"}]
                 (prob expgraph :a "on" #{}))))
    (is (= 0.3 (binding [params {:PriorFunc "foo"}]
                 (prob expgraph :a "on" #{}))))
    (is (= 0.2 (binding [params {:PriorFunc "max"}]
                 (prob expgraph :b "on" #{[:a "on"]}))))
    (is (= 0.2 (binding [params {:PriorFunc "min"}]
                 (prob expgraph :b "on" #{[:a "on"]}))))
    (is (= 0.2 (binding [params {:PriorFunc "avg"}]
                 (prob expgraph :b "on" #{[:a "on"]}))))
    (is (= 0.2 (binding [params {:PriorFunc "foo"}]
                 (prob expgraph :b "on" #{[:a "on"]}))))
    (is (= 0.4 (binding [params {:PriorFunc "max"}]
                 (prob expgraph :b "on" #{}))))
    (is (= 0.2 (binding [params {:PriorFunc "min"}]
                 (prob expgraph :b "on" #{}))))
    (is (> 0.001 (Math/abs (- 0.3 (binding [params {:PriorFunc "avg"}]
                                    (prob expgraph :b "on" #{}))))))
    (is (> 0.001 (Math/abs (- 0.3 (binding [params {:PriorFunc "foo"}]
                                    (prob expgraph :b "on" #{}))))))
    
    (is (= #{:b :c} (set (explains expgraph :a))))
    (is (empty? (explainers expgraph :a)))
    (is (= #{:a} (set (explainers expgraph :b))))))
