(ns retrospect.random
  (:import (org.apache.commons.math3.distribution NormalDistribution))
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

(defn my-rand-gauss
  [mean variance]
  (+ mean (* (Math/sqrt variance) (.nextGaussian rgen))))

(defn cumprob
  [mean variance lower upper]
  (.cumulativeProbability (NormalDistribution. mean (Math/sqrt variance)) lower upper))

(defn my-shuffle
  [coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rgen)
    (clojure.lang.RT/vector (.toArray al))))
