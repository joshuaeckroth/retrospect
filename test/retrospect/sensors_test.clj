(ns retrospect.sensors-test
  (:use [clojure.test])
  (:use [retrospect.sensors]))

(deftest test-sensing
  (let [sensor (init-sensor 1 (fn [s _ t] (add-sensed s t [(rand) (rand) (rand)]
                                                      [(+ 10.0 (rand)) (+ 10.0 (rand))])) {})
        sensor2 (first (update-sensors [sensor] nil 0))
        sensor3 (first (update-sensors [sensor2] nil 1))]
    (is (= 1 (:sensed-up-to sensor3)))
    (is (= 3 (count (get-in @(:sensed sensor3) [0 :observed]))))
    (is (= 3 (count (get-in @(:sensed sensor3) [1 :observed]))))
    (is (= 3 (count (sensed-at sensor3 1))))
    (is (= 2 (count (get-in @(:sensed sensor3) [0 :reserved]))))
    (is (= 2 (count (get-in @(:sensed sensor3) [1 :reserved]))))
    (is (every? #(<= % 1.0) (get-in @(:sensed sensor3) [0 :observed])))
    (is (every? #(<= % 1.0) (get-in @(:sensed sensor3) [1 :observed])))
    (is (every? #(>= % 10.0) (get-in @(:sensed sensor3) [0 :reserved])))
    (is (every? #(>= % 10.0) (get-in @(:sensed sensor3) [1 :reserved])))
    (do (sense-more-at sensor3 1)
        (is (= 5 (count (get-in @(:sensed sensor3) [1 :observed]))))
        (is (= 5 (count (sensed-at sensor3 1)))))))
