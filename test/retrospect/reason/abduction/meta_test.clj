(ns retrospect.reason.abduction.meta-test
  (:use [clojure.test])
  (:use [retrospect.problems.abdexp.problem :only [abdexp-problem]])
  (:use [retrospect.reason.abduction.reason :only [reason-abduction]])
  (:use [retrospect.reason.abduction.meta])
  (:use [retrospect.reason.abduction.workspace])
  (:use [retrospect.simulate])
  (:use [retrospect.epistemicstates])
  (:use [retrospect.state]))

(use-fixtures :each
  (fn [f]
    (dosync (alter logging-enabled (constantly false))
            (alter batch (constantly true)))
    (swap! conflicts-cache (constantly {}))
    (f)))

(deftest test-meta-no-offered-expl
  (dosync (alter reasoner (constantly reason-abduction))
          (alter problem (constantly
                          {:abduction {:hypothesize-fn (constantly [])
                                       :make-sensor-hyps-fn (constantly [])
                                       :ignore-doubt-types #{}}})))
  (binding [last-id 0
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
          ;; id 3
          h1 (new-hyp "Test" :type1 :mysubtype 0.25 false
                      conflicts?-fn [(:contents e1)]
                      "short-descr" "desc" {:x 1})
          ;; id 4
          h2 (new-hyp "Test" :type1 :mysubtype 0.50 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 2})
          ;; id 5
          h3 (new-hyp "Test" :type2 :mysubtype 0.60 false
                      conflicts?-fn [(:contents e2)]
                      "short-descr" "desc" {:x 3})
          ;; id 6
          h4 (new-hyp "Test" :type2 :mysubtype 0.40 false
                      conflicts?-fn [(:contents e1) (:contents e2)]
                      "short-descr" "desc" {:x 4})
          ;; id 7 (a noexp)
          ne1 (new-hyp "Test" :noexp :mysubtype 0.5 true
                       (constantly false) [] "" "" {:a 3})
          ws (-> (init-workspace)
                (add-observation e1 1)
                (add-observation e2 1)
                (add-observation ne1 1)
                (add h1 1)
                (add h2 1)
                (add h3 1)
                (add h4 1))
          sensors []
          time-prev 1
          time-now 2
          est (new-child-ep (new-child-ep (init-est ws)))
          est-expl-adv (explain-and-advance est time-prev time-now sensors)
          ws-expl-adv (:workspace (cur-ep est-expl-adv))]
      (is (accepted? ws-expl-adv e1))
      (is (accepted? ws-expl-adv e2))
      (is (accepted? ws-expl-adv h4))
      (is (not (accepted? ws-expl-adv h1)))
      (is (not (accepted? ws-expl-adv h2)))
      (is (not (accepted? ws-expl-adv h3)))
      (is (not (unexplained? ws-expl-adv e1)))
      (is (not (unexplained? ws-expl-adv e2)))
      (is (unexplained? ws-expl-adv ne1))
      (is (= [ne1] (no-explainers ws-expl-adv)))
      (is (= #{ne1} (find-problem-cases est-expl-adv)))
      (is (empty? (find-problem-cases
                   (binding [params (assoc params :Metareasoning "ignore")]
                     (metareason est-expl-adv time-prev time-now sensors)))))
      (is (= :ignoring (let [est-new (binding [params (assoc params :Metareasoning "ignore")]
                                       (metareason est-expl-adv time-prev time-now sensors))]
                         (rejection-reason (:workspace (cur-ep est-new)) ne1))))
      (is (empty? (find-problem-cases
                   (binding [params (assoc params :Metareasoning "batch1")]
                     (metareason est-expl-adv time-prev time-now sensors)))))
      (is (= #{ne1} (find-problem-cases
                     (:est-new (meta-batchbeg
                                #{ne1} est-expl-adv time-prev time-now sensors)))))
      (is (empty? (find-problem-cases
                   (binding [params (assoc params :Metareasoning "batchbeg")]
                     (metareason est-expl-adv time-prev time-now sensors)))))
      (is (empty? (find-problem-cases
                   (binding [params (assoc params :Metareasoning "lower-minscore")]
                     (metareason est-expl-adv time-prev time-now sensors)))))
      (is (= #{ne1} (find-problem-cases
                     (:est-new (meta-lower-minscore
                                #{ne1} est-expl-adv time-prev time-now sensors)))))
      (is (empty? (find-problem-cases
                   (binding [params (assoc params :Metareasoning "abd")]
                     (metareason est-expl-adv time-prev time-now sensors)))))
      (is (= #{ne1} (find-problem-cases
                     (:est-new (meta-abductive-recursive
                                #{ne1} est-expl-adv time-prev time-now sensors))))))))

