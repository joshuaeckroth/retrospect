(ns retrospect.reason.abduction.workspace-test
  (:use [clojure.test])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.simulate :only [get-default-params]])
  (:use [retrospect.state]))

(use-fixtures :each
  (fn [f]
    (dosync (alter logging-enabled (constantly false))
            (alter batch (constantly true)))
    (swap! cache (constantly {}))
    (f)))

(defn explain-helper
  [workspace]
  (loop [ws workspace]
    (let [ws-expl (explain ws 1 1)]
      (if (nil? (:best (:accrej ws-expl))) ws-expl
          (recur ws-expl)))))

(deftest test-new-hyp
  (binding [last-id 0]
    (let [h (new-hyp "Test" :mytype :mysubtype 0.25 true (constantly false)
                     [] "short-descr" "desc" {:x 1})]
      (is (= 1 (:id h)))
      (is (= "Test1" (:name h)))
      (is (= {:x 1 :type :mytype :subtype :mysubtype}
             (:contents h)))
      (is (= "Test1(short-descr)/0.25" (str h))))))

(deftest test-delta
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (get-default-params)]
    (let [h1 (new-hyp "Test" :mytype :mysubtype 0.25 true (constantly false)
                      [] "short-descr" "desc" {:x 1})
          h2 (new-hyp "Test" :mytype :mysubtype 0.50 true (constantly false)
                      [] "short-descr" "desc" {:x 1})
          h3 (new-hyp "Test" :mytype :mysubtype 0.60 true (constantly false)
                      [] "short-descr" "desc" {:x 1})
          h4 (new-hyp "Test" :mytype :mysubtype 0.40 true (constantly false)
                      [] "short-descr" "desc" {:x 1})]
      (is (= 1 (compare-by-delta (init-workspace) [h1 h2] [h3 h4])))
      (is (= -1 (compare-by-delta (init-workspace) [h3 h4] [h1 h2]))))))

(deftest test-minscore
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0 :MinScore 50)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          h1 (new-hyp "Test" :type1 :mysubtype 0.60 true
                      conflicts?-fn [] "short-descr" "desc" {:x 1})
          h2 (new-hyp "Test" :type1 :mysubtype 0.50 true
                      conflicts?-fn [] "short-descr" "desc" {:x 2})
          h3 (new-hyp "Test" :type2 :mysubtype 0.25 true
                      conflicts?-fn [] "short-descr" "desc" {:x 3})
          ws (-> (init-workspace)
                (add h1 1)
                (add h2 1)
                (add h3 1))]
      (is (undecided? ws h1))
      (is (rejected? ws h2))
      (is (= :minscore (rejection-reason ws h2)))
      (is (rejected? ws h3))
      (is (= :minscore (rejection-reason ws h3))))))

(deftest test-conflicts
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          h1 (new-hyp "Test" :type1 :mysubtype 0.25 true
                      conflicts?-fn [] "short-descr" "desc" {:x 1})
          h2 (new-hyp "Test" :type1 :mysubtype 0.50 true
                      conflicts?-fn [] "short-descr" "desc" {:x 2})
          h3 (new-hyp "Test" :type2 :mysubtype 0.60 true
                      conflicts?-fn [] "short-descr" "desc" {:x 3})
          h4 (new-hyp "Test" :type2 :mysubtype 0.40 true
                      conflicts?-fn [] "short-descr" "desc" {:x 4})
          ws (-> (init-workspace)
                (add h1 1)
                (add h2 1)
                (add h3 1)
                (add h4 1))]
      (is (= true (conflicts? h1 h2)))
      (is (= true (conflicts? h2 h1)))
      (is (= true (conflicts? h3 h4)))
      (is (= true (conflicts? h4 h3)))
      (is (= false (conflicts? h1 h1)))
      (is (= false (conflicts? h2 h2)))
      (is (= false (conflicts? h3 h3)))
      (is (= false (conflicts? h4 h4)))
      (is (= false (conflicts? h1 h3)))
      (is (= false (conflicts? h3 h1)))
      (is (= false (conflicts? h2 h4)))
      (is (= false (conflicts? h4 h2)))
      (is (= [h2] (find-conflicts-all ws h1)))
      (is (= [h1] (find-conflicts-all ws h2)))
      (is (= [h4] (find-conflicts-all ws h3)))
      (is (= [h3] (find-conflicts-all ws h4)))
      (is (= {0 {1 {1 false 2 true 3 false 4 false}
                 2 {1 true 2 false 3 false 4 false}
                 3 {1 false 2 false 3 false 4 true}
                 4 {1 false 2 false 3 true 4 false}}}
             (:conflicts @cache))))))

(deftest test-adding
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0 :MinScore 50)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          h1 (new-hyp "Test" :type1 :mysubtype 0.70 true
                      conflicts?-fn [] "short-descr" "desc" {:x 1})
          h2 (new-hyp "Test" :type1 :mysubtype 0.20 true
                      conflicts?-fn [] "short-descr" "desc" {:x 2})
          h3 (new-hyp "Test" :type1 :mysubtype 0.20 true
                      conflicts?-fn [] "short-descr" "desc" {:x 3})
          h4 (new-hyp "Test" :type1 :mysubtype 0.20 true
                      conflicts?-fn [] "short-descr" "desc" {:x 4})
          ws (-> (init-workspace)
                (add h1 1)
                (reject h1 :conflict 1)
                (add h1 1)
                (add h2 1)
                (add h2 1)
                (add h3 1)
                (add (assoc h3 :apriori 0.70) 1)
                (prevent-rejection h4 :minscore)
                (add h4 1))]
      (is (rejected? ws h1))
      (is (= :conflict (rejection-reason ws h1)))
      (is (rejected? ws h2))
      (is (= :minscore (rejection-reason ws h2)))
      (is (undecided? ws h3))
      (is (undecided? ws h4)))))

(deftest test-acceptance
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          h1 (new-hyp "Test" :type1 :mysubtype 0.25 true
                      conflicts?-fn [] "short-descr" "desc" {:x 1})
          h2 (new-hyp "Test" :type1 :mysubtype 0.50 true
                      conflicts?-fn [] "short-descr" "desc" {:x 2})
          h3 (new-hyp "Test" :type2 :mysubtype 0.60 true
                      conflicts?-fn [] "short-descr" "desc" {:x 3})
          h4 (new-hyp "Test" :type2 :mysubtype 0.40 true
                      conflicts?-fn [] "short-descr" "desc" {:x 4})
          h5 (new-hyp "Test" :type3 :mysubtype 0.40 true
                      conflicts?-fn [] "short-descr" "desc" {:x 5})
          ws (-> (init-workspace)
                (add h1 1)
                (add h2 1)
                (add h3 1)
                (add h4 1))
          ws-acc (accept ws h1 nil [] [] 0.0 nil 2)]
      (is (accepted? ws-acc h1))
      (is (= 2 (accepted-cycle ws-acc h1)))
      (is (rejected? ws-acc h2))
      (is (= 2 (rejected-cycle ws-acc h2)))
      (is (= :conflict (rejection-reason ws-acc h2)))
      (is (not (accepted? ws-acc h2)))
      (is (not (accepted? ws-acc h3)))
      (is (not (accepted? ws-acc h4)))
      (is (not (rejected? ws-acc h3)))
      (is (not (rejected? ws-acc h4)))
      ;; do not "accept" something that was never added
      (is (= ws-acc (accept ws-acc h5 nil [] [] 0.0 nil 2)))
      ;; do not "accept" something that was rejected
      (is (= ws-acc (accept ws-acc h2 nil [] [] 0.0 nil 2))))))

(deftest test-sorting
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h1 (new-hyp "Test" :type1 :mysubtype 0.25 true
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h2 (new-hyp "Test" :type1 :mysubtype 0.50 true
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h3 (new-hyp "Test" :type2 :mysubtype 0.60 true
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6
          h4 (new-hyp "Test" :type2 :mysubtype 0.40 true
                      conflicts?-fn [(:contents e1) (:contents e2)]
                      "short-descr" "desc" {:x 4})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add h1 1)
                (add h2 1)
                (add h3 1)
                (add h4 1))
          unexp (unexplained ws)]
      (is (= {:score false :expl false} (hyp-better-than? ws unexp h2 h3)))
      (is (= {:score true :expl false} (hyp-better-than? ws unexp h3 h2)))
      (is (= {:score false :expl true} (hyp-better-than? ws unexp h4 h3)))
      (is (= {:score true :expl false} (hyp-better-than? ws unexp h3 h4)))
      (is (= [{:hyp e1 :expl [h4 h1]} {:hyp e2 :expl [h3 h2 h4]}]))
      (let [normalized-aprioris [(/ 0.4 (+ 0.4 0.25)) (/ 0.25 (+ 0.4 0.25))]]
        (is (= {:best h4 :nbest h1 :delta (apply - normalized-aprioris)
                :normalized-aprioris normalized-aprioris
                :explained e1 :alts [h1] :comparison {:score true :expl true}}
               (find-best ws)))))))

(deftest test-explain
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h1 (new-hyp "Test" :type1 :mysubtype 0.25 true
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h2 (new-hyp "Test" :type1 :mysubtype 0.50 true
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h3 (new-hyp "Test" :type2 :mysubtype 0.60 true
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6
          h4 (new-hyp "Test" :type2 :mysubtype 0.40 true
                      conflicts?-fn [(:contents e1) (:contents e2)]
                      "short-descr" "desc" {:x 4})
          ;; id 7 (a noexp)
          ne1 (new-hyp "Test" :noexp :mysubtype 0.40 true
                       conflicts?-fn []
                       "short-descr" "desc" {:b 1})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add h1 1)
                (add h2 1)
                (add h3 1)
                (add h4 1)
                (add-observation ne1 1))
          ws-expl (explain ws 1 1)]
      (is (= [e1] (explains ws h1)))
      (is (= [e2] (explains ws h2)))
      (is (= [e2] (explains ws h3)))
      (is (= [e1 e2] (explains ws h4)))
      (is (= [h1 h4] (explainers ws e1)))
      (is (= [h2 h3 h4] (explainers ws e2)))
      (is (accepted? ws-expl e1))
      (is (accepted? ws-expl e2))
      (is (accepted? ws-expl h4))
      (is (not (accepted? ws-expl h1)))
      (is (not (accepted? ws-expl h2)))
      (is (not (accepted? ws-expl h3)))
      (is (rejected? ws-expl h3))
      (is (not (rejected? ws-expl h1)))
      (is (not (rejected? ws-expl h2)))
      (is (not (rejected? ws-expl h4)))
      (is (= #{h4 ne1} (set (unexplained ws-expl))))
      (is (unexplained? ws-expl h4))
      (is (not (unexplained? ws-expl e1)))
      (is (unexplained? ws-expl ne1))
      (is (= #{h4 ne1} (set (no-explainers ws-expl))))
      (is (= 0.5 (get-noexp-pct ws-expl)))
      (is (= #{h1 h2} (set (undecided ws-expl))))
      (is (= #{h4} (set (unexplained (reject ws-expl ne1 :ignoring 1))))))))

(deftest test-already-explained
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h3 (new-hyp "Test" :type1 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h4 (new-hyp "Test" :type2 :mysubtype 0.60 true
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5; repeat of h3, but now also explains h4
          h5 (new-hyp "Test" :type1 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1) (:contents h4)]
                      "short-descr" "desc" {:x 1})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add h3 1)
                (explain 1 1)
                (add-observation e2 2)
                (add h4 2)
                (add h5 2)
                (explain 2 2))]
      (is (accepted? ws h4))
      (is (not (unexplained? ws h4)))
      (is (empty? (unexplained ws))))))

(deftest test-composite
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h1 (new-hyp "Test" :type1 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h2 (new-hyp "Test" :type2 :mysubtype 0.50 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h3 (new-hyp "Test" :type2 :mysubtype 0.60 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6 (a composite of h1 and h2)
          hc1 (new-composite "TestComp" :type-comp1 :mysubtype 0.3
                             [(:contents e1) (:contents e2)]
                             "" "" {:c 1} [h1 h2])
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add h3 1)
                (add hc1 1))
          ws-expl (explain ws 1 1)
          ws-rej (-> ws (add h2 1) (reject h2 :conflict 1))
          ws-rej-expl (explain ws-rej 1 1)]
      (is (:composite? hc1))
      (is (= [h1 h2] (:hyps hc1)))
      (is (= [e2] (explains ws h3)))
      (is (= [e1 e2] (explains ws hc1)))
      (is (= [hc1] (explainers ws e1)))
      (is (= [h3 hc1] (explainers ws e2)))
      ;; after explaining
      (is (accepted? ws-expl e1))
      (is (accepted? ws-expl e2))
      (is (accepted? ws-expl hc1))
      (is (accepted? ws-expl h1))
      (is (accepted? ws-expl h2))
      (is (rejected? ws-expl h3))
      ;; rather than explaining, a preemptive rejection of a hyp in the composite
      (is (rejected? ws-rej h2))
      (is (rejected? ws-rej hc1))
      (is (= :conflict (rejection-reason ws-rej hc1)))
      (is (rejected? ws-rej-expl h2))
      (is (rejected? ws-rej-expl hc1))
      (is (accepted? ws-rej-expl h3)))))

(deftest test-undecide
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (assoc (get-default-params) :simulation 0)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h3 (new-hyp "Test" :type1 :mysubtype 0.25 true
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h4 (new-hyp "Test" :type2 :mysubtype 0.50 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h5 (new-hyp "Test" :type3 :mysubtype 0.60 true
                      conflicts?-fn [(:contents h3)]
                      "short-descr" "desc" {:x 3})
          ;; id 6
          h6 (new-hyp "Test" :type4 :mysubtype 0.40 false
                      conflicts?-fn [(:contents h5)]
                      "short-descr" "desc" {:x 4})
          ;; id 7
          h7 (new-hyp "Test" :type3 :mysubtype 0.40 false
                      conflicts?-fn []
                      "short-descr" "desc" {:x 5})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add h3 1)
                (add h4 1)
                (add h5 1)
                (add h6 1)
                (add h7 1))
          ws-expl (explain-helper ws)
          ws-undecided (undecide ws-expl h5)
          ws-undecided-expl (explain-helper ws-undecided)
          ws-undecided-expl-undecided (undecide ws-undecided-expl h5)]
      (is (= [1 3 5 6 7] (related-hyps ws e1)))
      (is (= [2 4] (related-hyps ws e2)))
      (is (= [3 5 6 7] (related-hyps ws h3)))
      (is (= [4] (related-hyps ws h4)))
      (is (= [5 6 7] (related-hyps ws h5)))
      (is (= [6] (related-hyps ws h6)))
      (is (= [7 5 6] (related-hyps ws h7)))
      (is (accepted? ws-expl e1))
      (is (accepted? ws-expl e2))
      (is (accepted? ws-expl h3))
      (is (accepted? ws-expl h4))
      (is (accepted? ws-expl h5))
      (is (accepted? ws-expl h6))
      (is (rejected? ws-expl h7))
      (is (= :conflict (rejection-reason ws-expl h7)))
      (is (accepted? ws-undecided e1))
      (is (accepted? ws-undecided e2))
      (is (accepted? ws-undecided h3))
      (is (accepted? ws-undecided h4))
      (is (undecided? ws-undecided h5))
      (is (undecided? ws-undecided h6))
      (is (undecided? ws-undecided h7))
      (is (= [h3] (unexplained ws-undecided)))
      (is (= ws-expl ws-undecided-expl))
      (is (= ws-undecided ws-undecided-expl-undecided)))))
