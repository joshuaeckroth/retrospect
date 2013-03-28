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


