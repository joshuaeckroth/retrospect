(ns retrospect.problems.words.learning
  (:use [retrospect.profile :only [prof]])
  (:use [retrospect.state]))

(defn get-letter-trans-freqs
  [word n]
  (let [gs (partition n 1 (seq word))]
    (reduce (fn [m g] (if (get m g) (update-in m [g] + 1.0)
                          (assoc m g 1.0)))
            {} gs)))

(defn normalize
  [m]
  (let [s (reduce + (vals m))]
    (reduce (fn [m2 g] (assoc m2 g (double (/ (get m2 g) s))))
            m (keys m))))

(defn get-features
  [word]
  (normalize (get-letter-trans-freqs word (:SymbolGrams params))))

(defn update-features
  [features dictionary]
  (reduce (fn [f w] (if (get f w) f (assoc f w (get-features w))))
          features dictionary))

(defn calc-symbol-ngram
  [features unigram-model]
  (reduce (fn [m m2] (reduce (fn [m g] (assoc m g (+ (get m g 0.0)
                                                     (get m2 g))))
                             m (keys m2)))
          {} (apply concat (for [w (keys features)]
                             (repeat (get unigram-model [w])
                                     (get features w))))))

(defn get-similar-features
  [word features ngram]
  (let [ngram-keys (prof :ngram-keys
                         (mapcat (fn [f] (filter (fn [f2] (or (= (first (seq f2))
                                                                 (first (seq word)))
                                                              (= (last (seq f2))
                                                                 (last (seq word)))))
                                                 (keys ngram)))
                                 (keys features)))]
    (prof :get-similar-features
          (normalize
           (prof :get-similar-merge
                 (reduce (fn [m k] (assoc m k (get ngram k 0.0))) {}
                         (concat (keys features) ngram-keys)))))))

(comment (get-similar-features word fs ngram))

(defn similarity
  [word ngram]
  (let [fs (get-features word)
        similar (normalize (select-keys ngram (keys fs)))]
    (reduce (fn [s f] (+ s (* (get similar f 0.0) (get fs f)))) 0.0 (keys fs))))
