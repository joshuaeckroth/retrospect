(ns retrospect.random
  (:import (java.util Random)))

(def rgen nil)

(defn new-seed
  [n]
  (Random. n))

(defn my-rand
  ([] (.nextDouble rgen))
  ([n] (* (double n) (my-rand))))

(defn my-rand-int
  [n]
  (int (my-rand n)))

(defn my-rand-nth
  [coll]
  (nth coll (my-rand-int (count coll))))

(defn my-shuffle
  [coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rgen)
    (clojure.lang.RT/vector (.toArray al))))
