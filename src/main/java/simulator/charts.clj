(ns simulator.charts
  (:use [incanter.core])
  (:use [incanter.charts])
  (:use [incanter.io])
  (:use [incanter.stats :as stats])
  (:use [simulator.strategies :only (strategies)]))

(defn get-strat-regression
  [strategy meta-abduce data x y strategy-regression]
  (case strategy-regression
        :linear
        (merge
         {:strategy strategy :reg-type "Linear"
          :xs (sel ($where {:Strategy strategy :MetaAbduce meta-abduce} data) :cols x)}
         (try (stats/linear-model
               (sel ($where {:Strategy strategy :MetaAbduce meta-abduce} data) :cols y)
               (sel ($where {:Strategy strategy :MetaAbduce meta-abduce} data) :cols x))
              (catch Exception e {})))))

(defn plot
  [data x y y-range regression strategy-regression]
  (with-data data
    (let [strats-with-data (filter
                            (fn [s] (< 1 (nrow ($where {:Strategy s :MetaAbduce "false"}))))
                            strategies)
          strat-regs (doall (map #(if strategy-regression
                                    (get-strat-regression % "false" data x y
                                                          strategy-regression))
                                 strats-with-data))
          plot (doto (scatter-plot x y :x-label (name x) :y-label (name y)
                                   :data ($where {:Strategy (first strats-with-data)
                                                  :MetaAbduce "false"})
                                   :legend true
                                   :series-label (first strats-with-data))
                 (clear-background))
          plot2 (reduce (fn [p strategy]                          
                          (if (= 1 (nrow ($where {:Strategy strategy :MetaAbduce "false"}))) p
                              (add-points p x y
                                          :data ($where {:Strategy strategy
                                                         :MetaAbduce "false"})
                                          :series-label strategy)))
                        plot (rest strats-with-data))
          plot3 (if y-range (apply set-y-range plot2 y-range) plot2)
          plot4 (if (nil? strategy-regression) plot3
                    (reduce (fn [p strat-reg]
                              (if (nil? (:fitted strat-reg)) p
                                  (add-lines p (:xs strat-reg) (:fitted strat-reg)
                                             :series-label
                                             (format "%s reg for %s, r^2=%.2f"
                                                     (:reg-type strat-reg)
                                                     (:strategy strat-reg)
                                                     (:r-square strat-reg)))))
                            plot3 strat-regs))]
      (cond (nil? regression)
            plot4
            (= regression :linear)
            (let [lm (stats/linear-model (sel data :cols y)
                                         (sel data :cols x))]
              (add-lines plot3 (sel data :cols x) (:fitted lm)
                         :series-label (format "Linear reg, r^2=%.2f"
                                               (:r-square lm))))))))

(defn read-results [filename]
  (read-dataset filename :header true))

(defn save-plots
  [recordsdir problem]
  (let [results (read-results (str recordsdir "/results.csv"))]
    (doseq [c (:charts problem)]
      (if (:split-by c)
        (doseq [split (:split-list c)]
          (let [data ($where {(:split-by c) {:$gt (- split (:split-delta c))
                                             :$lt (+ split (:split-delta c))}}
                             results)]
            (when (not-empty (sel data :cols (:x c)))
              (save (plot data (:x c) (:y c) (:y-range c)
                          (:regression c) (:strategy-regression c))
                    (format "%s/%s-%s--%s-%s.png" recordsdir
                            (:name problem) (:name c)
                            (name (:split-by c)) (str split))
                    :width 800 :height 600))))
        (save (plot results (:x c) (:y c) (:y-range c)
                    (:regression c) (:strategy-regression c))
              (format "%s/%s-%s.png" recordsdir
                      (:name problem) (:name c))
              :width 800 :height 600)))))
