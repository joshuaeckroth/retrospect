(ns retrospect.problems.words.learning
  (:use [retrospect.state]))

(defn get-letter-trans-freqs
  [word n]
  (let [gs (partition n 1 (seq word))]
    (reduce (fn [m g] (if (get m g) (update-in m [g] inc)
                          (assoc m g 1)))
            {} gs)))

(defn normalize
  [m]
  (let [s (reduce + (vals m))]
    (reduce (fn [m2 g] (assoc m2 g (double (/ (get m2 g) s))))
            m (keys m))))

(defn get-features
  [word]
  (normalize (apply merge (map #(get-letter-trans-freqs word %)
                               (range 1 (inc (:LearnFeatureSize params)))))))

(defn update-features
  [features dictionary]
  (reduce (fn [f w] (if (get f w) f
                        (assoc f w (get-features w))))
          features dictionary))

(defn calc-centroid
  [features unigram-model]
  (let [sum (reduce (fn [m m2] (reduce (fn [m g] (assoc m g (+ (get m g 0.0)
                                                               (get m2 g))))
                                       m (keys m2)))
                    {} (apply concat (for [w (keys features)]
                                       (repeat (get unigram-model [w])
                                               (get features w)))))]
    (normalize sum)))

(defn similarity
  [word centroid]
  (let [fs (get-features word)]
    (reduce (fn [s g] (+ s (* (get centroid g 0.0) (get fs g)))) 0.0 (keys fs))))
