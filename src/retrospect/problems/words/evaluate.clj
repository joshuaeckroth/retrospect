(ns retrospect.problems.words.evaluate
  (:use [clojure.contrib.map-utils :only [deep-merge-with]]))

(defn levenshtein-distance
  [a b]
  (let [m (count a)
        n (count b)
        init (apply deep-merge-with (fn [a b] b)
                    (concat
                      ;;deletion
                      (for [i (range 0 (+ 1 m))]
                        {i {0 i}})
                      ;;insertion
                      (for [j (range 0 (+ 1 n))]
                        {0 {j j}})))
        table (reduce
                (fn [d [i j]]
                  (deep-merge-with
                    (fn [a b] b)
                    d
                    {i {j (if (= (nth a (- i 1))
                                 (nth b (- j 1)))
                            ((d (- i 1)) (- j 1))
                            (min
                              (+ ((d (- i 1))
                                  j) 1) ;;deletion
                              (+ ((d i)
                                  (- j 1)) 1) ;;insertion
                              (+ ((d (- i 1))
                                  (- j 1)) 1))) ;;substitution
                    }}))
                init
                (for [j (range 1 (+ 1 n))
                        i (range 1 (+ 1 m))] [i j]))]
    ((table m) n)))

(defn get-truewords
  [truedata time]
  (loop [ws [] td (:words (meta truedata))]
    (if (< time (+ (reduce + 0 (map count ws)) (count (first td)))) ws 
      (recur (conj ws (first td)) (rest td)))))

(defn evaluate
  [ep-state results prev-ep sensors truedata params]
  (let [time (:time ep-state)
        truewords (get-truewords truedata (:time ep-state))]
    {:LD (levenshtein-distance (:history (:problem-data ep-state)) truewords)}))

(defn avg-with-prior
  [results key val]
  (let [c (count results)]
    (cond (= c 0) val
          (= 0.0 val) (key (last results))
          :else (double (/ (+ (* c (key (last results))) val) (inc c))))))

(defn evaluate-meta
  [ep-state meta-ep-state meta-accepted-type results truedata params]
  (let [history (:history (:problem-data ep-state))
        history-meta (:history (:problem-data meta-ep-state))
        truewords (get-truewords truedata (:time ep-state))
        ld (levenshtein-distance history truewords)
        ld-meta (levenshtein-distance history-meta truewords)]
    {:AvgMetaDiffLD 
     (avg-with-prior results (keyword (format "%s%s" (name meta-accepted-type) "AvgMetaDiffLD"))
                     (- ld-meta ld))}))

(defn calc-percent-increase
  [k m b]
  (if (= 0 (k b)) 0.0
    (double (* 100.0 (/ (- (k m) (k b)) (k b))))))

(defn calc-ratio
  [k m b]
  (if (= 0 (k b)) 0.0
    (double (/ (k m) (k b)))))

(defn evaluate-comparative
  [params [m b]]
  {:MetaLD (:LD m)
   :BaseLD (:LD b)
   :RatioLD (calc-ratio :LD m b)
   :IncreaseLD (calc-percent-increase :LD m b)
   :MaxModelGrams (:MaxModelGrams params)})
