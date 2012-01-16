(ns retrospect.explore
  (:use [retrospect.random :only [rgen new-seed my-rand-nth my-rand-int my-rand]])
  (:use [retrospect.problem :only [get-default-params-ranges run-simulation]])
  (:use [retrospect.onerun :only [init-one-run-state]])
  (:use [retrospect.workspaces :only [last-id]])
  (:use [retrospect.state]))

(def attempted (atom {}))

(defn search
  [scorer permute state min-max permutations]
  (let [init state
        init-score (scorer state)
        comp (if (= "min" min-max) < >)
        opt (if (= "min" min-max) min max)]
    (loop [t 1
           state init
           score init-score
           best init
           best-score init-score]
      (if (> t permutations) {:best best :best-score best-score}
          (do
            (println "Permutation" t "of" permutations)
            (let [next (permute state)
                  next-score (scorer next)
                  better? (comp next-score score)]
              (when better?
                (println "New best score:" next-score "for" next))
              (if better?
                (recur (inc t) next next-score
                       (if (comp next-score best-score) next best)
                       (opt next-score best-score))
                (recur (inc t) state score best best-score))))))))

(defn run
  [metric params repetitions]
  (println "Simulating" (pr-str params))
  (let [rs (for [i (range repetitions)]
             (let [seed (my-rand-int 10000000)]
               (binding [rgen (new-seed seed)
                         last-id 0
                         retrospect.state/params (assoc params :Seed seed)]
                 (println "Seed:" seed)
                 (let [truedata ((:truedata-fn @problem))
                       sensors ((:sensor-gen-fn @problem))
                       problem-data ((:gen-problem-data-fn @problem) truedata sensors)
                       or-state (init-one-run-state sensors problem-data)
                       results (run-simulation truedata or-state false)]
                   (println (format "%s = %s" (name metric) (get (last results) metric)))
                   results))))
        avg (double (/ (reduce + (map #(get (last %) metric) rs)) repetitions))]
    (swap! attempted assoc params rs)
    (println "Average =" avg)
    avg))

(defn next-params
  [def-ps last-params]
  (loop []
    (let [field (my-rand-nth (filter #(second (get def-ps %)) (keys def-ps)))
          val (my-rand-nth (get def-ps field))
          params (assoc last-params field val)]
      (println "\nSwapping" field "with" val "\n")
      (if (or (= (get last-params field) val) (some #{params} (keys @attempted)))
        (recur) params))))

(defn explore
  [seed metric min-max repetitions restarts permutations]
  (binding [rgen (new-seed seed)]
    (let [def-ps (get-default-params-ranges)
          bests (loop [i restarts
                       bests []]
                  (if (> 0 i) bests
                      (let [first-ps (reduce (fn [m k]
                                               (assoc m k (my-rand-nth (get def-ps k))))
                                             {} (keys def-ps))
                            ;; do a permutation to ensure the first chosen
                            ;; was not already attempted
                            ps (next-params def-ps first-ps)]
                        (println "Restart number" (- restarts i))
                        (recur (dec i) (conj bests (search #(run metric % repetitions)
                                                           (partial next-params def-ps)
                                                           ps min-max permutations))))))
          best (first (if (= "max" min-max) (reverse (sort-by :best-score bests))
                          (sort-by :best-score bests)))]
      (println "Restarting from best...")
      (println best)
      (search #(run metric % repetitions)
              (partial next-params def-ps)
              (:best best) min-max permutations))))
