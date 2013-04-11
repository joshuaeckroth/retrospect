(ns retrospect.reason.abduction.workspace-test
  (:use [clojure.test])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.simulate :only [get-default-params]])
  (:use [retrospect.state]))

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

(deftest test-conflicts
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (get-default-params)]
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
      (is (= [h3] (find-conflicts-all ws h4))))))

(deftest test-acceptance
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (get-default-params)]
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
            params (get-default-params)]
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
          ws-sorted (update-sorted-explainers ws)]
      (is (= {:score false :expl false} (hyp-better-than? ws-sorted h2 h3)))
      (is (= {:score true :expl false} (hyp-better-than? ws-sorted h3 h2)))
      (is (= {:score false :expl true} (hyp-better-than? ws-sorted h4 h3)))
      (is (= {:score true :expl false} (hyp-better-than? ws-sorted h3 h4)))
      (is (= {1 [6 3] 2 [5 4 6]} (:sorted-explainers ws-sorted)))
      ;; delta of 6-3 is 0.15, delta of 5-4 is 0.1
      (is (= [1 2] (:sorted-explainers-explained ws-sorted)))
      (let [normalized-aprioris [(/ 0.4 (+ 0.4 0.25)) (/ 0.25 (+ 0.4 0.25))]]
        (is (= {:best h4 :nbest h1 :delta (apply - normalized-aprioris)
                :normalized-aprioris normalized-aprioris
                :explained e1 :alts [h1] :comparison {:score true :expl true}}
               (find-best ws-sorted)))))))

(deftest test-explain
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly abdexp-problem)))
  (binding [last-id 0
            params (get-default-params)]
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
      (is (= [6 7] (get-unexplained ws-expl)))
      (is (unexplained? ws-expl h4))
      (is (not (unexplained? ws-expl e1)))
      (is (unexplained? ws-expl ne1))
      (is (= [6 7] (get-no-explainers ws-expl)))
      (is (= 0.5 (get-noexp-pct ws-expl)))
      (is (= #{3 4 5} (find-unaccepted ws-expl))))))
