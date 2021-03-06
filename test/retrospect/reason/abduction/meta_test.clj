(ns retrospect.reason.abduction.meta-test
  (:use [clojure.test])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.reason.abduction.meta])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.simulate])
  (:use [retrospect.epistemicstates])
  (:use [retrospect.state])
  (:use [geppetto.random]))

(use-fixtures :each
  (fn [f]
    (dosync (alter logging-enabled (constantly false))
            (alter batch (constantly true)))
    (swap! cache (constantly {}))
    (f)))

(deftest test-meta-no-offered-expl
  (binding [last-id 0
            rgen (new-seed 0)
            params (assoc (get-default-params)
                     :GetMoreHyps false)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "NOETest" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "NOETest" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h3 (new-hyp "NOETest" :type1 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h4 (new-hyp "NOETest" :type1 :mysubtype 0.50 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h5 (new-hyp "NOETest" :type2 :mysubtype 0.60 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6
          h6 (new-hyp "NOETest" :type2 :mysubtype 0.40 false
                      conflicts?-fn [(:contents e1) (:contents e2)]
                      "short-descr" "desc" {:x 4})
          ;; id 7 (a noexp)
          ne7 (new-hyp "NOETest" :noexp :mysubtype 0.5 true
                       (constantly false) [] "" "" {:a 3})
          ;; id 8 (explains ne1)
          h8 (new-hyp "NOEcTest" :type3 :mysubtype 0.40 false
                      conflicts?-fn [(:contents ne7)]
                      "short-descr" "desc" {:x 5})
          sensor-hyps-fn (fn [sensors time-prev time-now accepted hypotheses]
                           (cond (and (= 0 time-prev) (= 1 time-now)) [e1 e2]
                                 (and (= 0 time-prev) (= 2 time-now)) [e1 e2 ne7]
                                 (and (= 1 time-prev) (= 2 time-now)) [ne7]
                                 :else []))
          hyps-fn (fn [unexp accepted hypotheses time-now]
                    (cond
                     (= 1 time-now) [h3 h4 h5 h6]
                     ;; h6 has been accepted; don't offer h8
                     (and (= 2 time-now) (= [h6] (:type2 accepted)))
                     [h3 h4 h5 h6]
                     :else
                     [h3 h4 h5 h6 h8]))]
      (dosync (alter reasoner (constantly reason-abduction))
              (alter problem (constantly
                              {:abduction {:hypothesize-fn hyps-fn
                                           :make-sensor-hyps-fn sensor-hyps-fn
                                           :ignore-doubt-types #{}}})))
      (let [sensors []
            est (update-est-ep (new-child-ep (new-child-ep (init-est (init-workspace))))
                               :time 1)
            est2 (update-est-ep (new-child-ep (explain-and-advance est 0 1 sensors))
                                :time 2)
            est-expl-adv (explain-and-advance est2 1 2 sensors)
            ws-expl-adv (:workspace (cur-ep est-expl-adv))
            time-prev 1
            time-now 2]
        (is (accepted? ws-expl-adv e1))
        (is (accepted? ws-expl-adv e2))
        (is (accepted? ws-expl-adv h6))
        (is (not (accepted? ws-expl-adv h3)))
        (is (not (accepted? ws-expl-adv h4)))
        (is (not (accepted? ws-expl-adv h5)))
        (is (not (unexplained? ws-expl-adv e1)))
        (is (not (unexplained? ws-expl-adv e2)))
        (is (unexplained? ws-expl-adv ne7))
        (is (= [ne7] (no-explainers ws-expl-adv)))
        (is (= #{ne7} (find-problem-cases est-expl-adv)))
        (is (empty? (find-problem-cases
                     (binding [params (assoc params :Metareasoning "ignore")]
                       (metareason est-expl-adv time-prev time-now sensors)))))
        (is (= :ignoring (let [est-new (binding [params (assoc params :Metareasoning "ignore")]
                                         (metareason est-expl-adv time-prev time-now sensors))]
                           (rejection-reason (:workspace (cur-ep est-new)) ne7))))
        (is (= #{ne7} (find-problem-cases
                       (:est-new (meta-batch1 #{ne7} est-expl-adv time-prev time-now sensors)))))
        (is (empty? (find-problem-cases
                     (:est-new (meta-batchbeg
                                #{ne7} est-expl-adv time-prev time-now sensors)))))
        (is (empty? (find-problem-cases
                     (binding [params (assoc params :Metareasoning "batchbeg")]
                       (metareason est-expl-adv time-prev time-now sensors)))))
        (is (empty? (find-problem-cases
                     (binding [params (assoc params :Metareasoning "lower-minscore")]
                       (metareason est-expl-adv time-prev time-now sensors)))))
        (is (= #{ne7} (find-problem-cases
                       (:est-new (meta-lower-minscore
                                  #{ne7} est-expl-adv time-prev time-now sensors)))))
        (is (empty? (find-problem-cases
                     (binding [params (assoc params :Metareasoning "abd")]
                       (metareason est-expl-adv time-prev time-now sensors)))))
        (is (= #{0 2} (set (map :cycle (make-meta-hyps-order-dep
                                      #{ne7} est-expl-adv time-prev time-now #{"meta-order-dep"})))))
        (is (empty? (find-problem-cases
                     (:est-new (meta-abductive-recursive
                                #{ne7} est-expl-adv time-prev time-now sensors)))))))))

(deftest test-meta-rej-conflict
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly
                          {:abduction {:hypothesize-fn (constantly [])
                                       :make-sensor-hyps-fn (constantly [])
                                       :ignore-doubt-types #{}}})))
  (binding [last-id 0
            rgen (new-seed 0)
            params (assoc (get-default-params)
                     :GetMoreHyps false)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3 (accepted first, rejects h5/h6)
          h3 (new-hyp "Test" :type1 :mysubtype 0.75 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h4 (new-hyp "Test" :type2 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h5 (new-hyp "Test" :type1 :mysubtype 0.60 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6
          h6 (new-hyp "Test" :type1 :mysubtype 0.50 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 4})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add h3 1)
                (add h4 1)
                (add h5 1)
                (add h6 1))
          sensors []
          time-prev 1
          time-now 2
          est (update-est-ep (new-child-ep (new-child-ep (init-est ws)))
                             :time 2)
          est-expl-adv (explain-and-advance est time-prev time-now sensors)
          ws-expl-adv (:workspace (cur-ep est-expl-adv))
          {est-rc-old :est-old est-rc-new :est-new} (meta-rej-conflict
                                                     #{e2} est-expl-adv time-prev time-now sensors)
          ws-rc-old (:workspace (cur-ep est-rc-old))
          ws-rc-new (:workspace (cur-ep est-rc-new))]
      (is (accepted? ws-expl-adv e1))
      (is (accepted? ws-expl-adv e2))
      (is (accepted? ws-expl-adv h3))
      (is (not (accepted? ws-expl-adv h4)))
      (is (not (accepted? ws-expl-adv h5)))
      (is (not (accepted? ws-expl-adv h6)))
      (is (undecided? ws-expl-adv h4))
      (is (rejected? ws-expl-adv h5))
      (is (rejected? ws-expl-adv h6))
      (is (= :conflict (rejection-reason ws-expl-adv h5)))
      (is (= :conflict (rejection-reason ws-expl-adv h6)))
      (is (not (unexplained? ws-expl-adv e1)))
      (is (unexplained? ws-expl-adv e2))
      (is (= [e2] (no-explainers ws-expl-adv)))
      (is (= #{e2} (find-problem-cases est-expl-adv)))
      (is (= #{{:implicated h3 :cycle 2 :expl-conflicting [h5 h6] :delta 0.5 :may-resolve [e2]}}
             (find-rej-conflict-candidates #{e2} est-expl-adv time-now)))
      (is (unexplained? ws-rc-old e2))
      (is (rejected? ws-rc-new h3))
      (is (= :preemptive (rejection-reason ws-rc-new h3)))
      (is (accepted? ws-rc-new h4))
      (is (accepted? ws-rc-new h5))
      (is (empty? (unexplained ws-rc-new))))))

(deftest test-meta-lower-minscore
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly
                          {:abduction {:hypothesize-fn (constantly [])
                                       :make-sensor-hyps-fn (constantly [])
                                       :ignore-doubt-types #{}}})))
  (binding [last-id 0
            rgen (new-seed 0)
            params (assoc (get-default-params)
                     :GetMoreHyps false :MinScore 50)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.75 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          h2 (new-hyp "Test" :type1 :mysubtype 0.35 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 3
          h3 (new-hyp "Test" :type1 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 2})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add h2 1)
                (add h3 1))
          sensors []
          time-prev 1
          time-now 2
          est (new-child-ep (new-child-ep (init-est ws)))
          est-expl-adv (explain-and-advance est time-prev time-now sensors)
          ws-expl-adv (:workspace (cur-ep est-expl-adv))
          {est-rc-old :est-old est-rc-new :est-new} (meta-lower-minscore
                                                     #{e1} est-expl-adv time-prev time-now sensors)
          ws-rc-old (:workspace (cur-ep est-rc-old))
          ws-rc-new (:workspace (cur-ep est-rc-new))]
      (is (accepted? ws-expl-adv e1))
      (is (rejected? ws-expl-adv h2))
      (is (rejected? ws-expl-adv h3))
      (is (= :minscore (rejection-reason ws-expl-adv h2)))
      (is (= :minscore (rejection-reason ws-expl-adv h3)))
      (is (unexplained? ws-expl-adv e1))
      (is (= [e1] (no-explainers ws-expl-adv)))
      (is (= #{e1} (find-problem-cases est-expl-adv)))
      (is (unexplained? ws-rc-old e1))
      (is (accepted? ws-rc-new h2))
      (is (rejected? ws-rc-new h3))
      (is (= :minscore (rejection-reason ws-rc-new h3)))
      (is (prevented-rejection? ws-rc-new h2 :minscore))
      (is (empty? (unexplained ws-rc-new))))))

(deftest test-meta-compound-cause
  ;; the explainer is rejected due to minscore, but when that's fixed,
  ;; it's rejected due to conflict
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly
                          {:abduction {:hypothesize-fn (constantly [])
                                       :make-sensor-hyps-fn (constantly [])
                                       :ignore-doubt-types #{}}})))
  (binding [last-id 0
            rgen (new-seed 0)
            params (assoc (get-default-params)
                     :GetMoreHyps false
                     :MinScore 20)]
    (let [conflicts?-fn (fn [hyp1 hyp2] (and (not= hyp1 hyp2)
                                            (= (:type hyp1) (:type hyp2))))
          ;; id 1
          e1 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 1})
          ;; id 2
          e2 (new-hyp "Test" :explained :mysubtype 0.5 true
                      (constantly false) [] "" "" {:a 2})
          ;; id 3
          h3 (new-hyp "Test" :type1 :mysubtype 0.50 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4 (competes with h3 as explainer; not chosen due to score)
          h4 (new-hyp "Test" :type2 :mysubtype 0.40 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 2})
          ;; id 5 (will be rejected first due to minscore, but also due to conflict with h3)
          h5 (new-hyp "Test" :type1 :mysubtype 0.15 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6 (will be rejected first due to minscore, but also due to conflict with h3)
          h6 (new-hyp "Test" :type1 :mysubtype 0.14 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 4})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add h3 1)
                (add h4 1)
                (add h5 1)
                (add h6 1))
          sensors []
          time-prev 1
          time-now 2
          est (update-est-ep (new-child-ep (new-child-ep (init-est ws)))
                             :time 2)
          est-expl-adv (explain-and-advance est time-prev time-now sensors)
          ws-expl-adv (:workspace (cur-ep est-expl-adv))
          est-meta-ignore (binding [params (assoc params :Metareasoning "ignore")]
                            (metareason est-expl-adv time-prev time-now sensors))
          ws-meta-ignore (:workspace (cur-ep est-meta-ignore))
          est-meta-batchbeg (binding [params (assoc params :Metareasoning "batchbeg")]
                              (metareason est-expl-adv time-prev time-now sensors))
          ws-meta-batchbeg (:workspace (cur-ep est-meta-batchbeg))
          est-meta-batchbeg-tmp (:est-new (meta-batchbeg #{e2} est-expl-adv
                                                         time-prev time-now sensors))
          ws-meta-batchbeg-tmp (:workspace (cur-ep est-meta-batchbeg-tmp))
          est-meta-lms (binding [params (assoc params :Metareasoning "lower-minscore")]
                         (metareason est-expl-adv time-prev time-now sensors))
          ws-meta-lms (:workspace (cur-ep est-meta-batchbeg))
          est-meta-lms-tmp (:est-new (meta-lower-minscore #{e2} est-expl-adv
                                                          time-prev time-now sensors))
          ws-meta-lms-tmp (:workspace (cur-ep est-meta-lms-tmp))
          est-meta-rc (binding [params (assoc params :Metareasoning "rej-conflict")]
                        (metareason est-expl-adv time-prev time-now sensors))
          ws-meta-rc (:workspace (cur-ep est-meta-rc))
          est-meta-rc-tmp (:est-new (meta-rej-conflict #{e2} est-expl-adv
                                                       time-prev time-now sensors))
          ws-meta-rc-tmp (:workspace (cur-ep est-meta-rc-tmp))
          est-meta-abd (binding [params (assoc params :Metareasoning "abd")]
                         (metareason est-expl-adv time-prev time-now sensors))
          ws-meta-abd (:workspace (cur-ep est-meta-abd))
          est-meta-abd-tmp (:est-new (meta-abductive-recursive #{e2} est-expl-adv
                                                               time-prev time-now sensors))
          ws-meta-abd-tmp (:workspace (cur-ep est-meta-abd-tmp))]
      (is (accepted? ws-expl-adv e1))
      (is (accepted? ws-expl-adv e2))
      (is (accepted? ws-expl-adv h3))
      (is (not (accepted? ws-expl-adv h4)))
      (is (not (accepted? ws-expl-adv h5)))
      (is (not (accepted? ws-expl-adv h6)))
      (is (not (rejected? ws-expl-adv h4))) ;; h6 not accepted, so h4 not rejected
      (is (rejected? ws-expl-adv h5))
      (is (= :minscore (rejection-reason ws-expl-adv h5)))
      (is (rejected? ws-expl-adv h6))
      (is (= :minscore (rejection-reason ws-expl-adv h6)))
      (is (unexplained? ws-expl-adv e2))
      (is (= [e2] (no-explainers ws-expl-adv)))
      (is (= #{e2} (find-problem-cases est-expl-adv)))
      (is (empty? (find-problem-cases est-meta-ignore)))
      (is (= :ignoring (rejection-reason ws-meta-ignore e2)))

      ;; batchbeg just falls back to ignoring
      (is (empty? (find-problem-cases est-meta-batchbeg)))
      (is (accepted? ws-meta-batchbeg h3))
      (is (= :minscore (rejection-reason ws-meta-batchbeg h5)))
      (is (= :minscore (rejection-reason ws-meta-batchbeg h6)))
      (is (= :ignoring (rejection-reason ws-meta-batchbeg e2)))
      (is (= #{e2} (find-problem-cases est-meta-batchbeg-tmp)))
      (is (accepted? ws-meta-batchbeg-tmp h3))
      (is (= :minscore (rejection-reason ws-meta-batchbeg-tmp h5)))
      (is (= :minscore (rejection-reason ws-meta-batchbeg-tmp h6)))
      (is (unexplained? ws-meta-batchbeg-tmp e2))

      ;; lower-minscore fixes the issue by preventing h5 from being
      ;; rejected due to minscore, thus leaving it an essential
      ;; explainer for e2, thus rejecting h3, and leaving h4 as an
      ;; essential for e1; h6 is left rejected due to minscore
      (is (empty? (find-problem-cases est-meta-lms)))
      (is (accepted? ws-meta-lms h3))
      (is (= :minscore (rejection-reason ws-meta-lms h5)))
      (is (= :minscore (rejection-reason ws-meta-lms h6)))
      (is (= :ignoring (rejection-reason ws-meta-lms e2)))
      (is (empty? (find-problem-cases est-meta-lms-tmp)))
      (is (accepted? ws-meta-lms-tmp h5))
      (is (accepted? ws-meta-lms-tmp h4))
      (is (rejected? ws-meta-lms-tmp h3))
      (is (= :conflict (rejection-reason ws-meta-lms-tmp h3)))
      (is (= :minscore (rejection-reason ws-meta-lms-tmp h6)))

      ;; reject-conflict just falls back to ignoring because, of course,
      ;; it does not solve the minscore issue (which was the "original" cause)
      (is (empty? (find-problem-cases est-meta-rc)))
      (is (accepted? ws-meta-rc h3))
      (is (= :minscore (rejection-reason ws-meta-rc h5)))
      (is (= :minscore (rejection-reason ws-meta-rc h6)))
      (is (= :ignoring (rejection-reason ws-meta-rc e2)))
      (is (= #{e2} (find-problem-cases est-meta-rc-tmp)))

      ;; what does abd do? or should it do?
      )))

