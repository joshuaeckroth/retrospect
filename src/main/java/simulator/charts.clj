(ns simulator.charts
  (:use [incanter.core])
  (:use [incanter.charts])
  (:use [incanter.io])
  (:use [incanter.stats :as stats])
  (:use [simulator.strategies.composite :only [strategies strategy-info]])
  (:use [simulator.strategies.metastrategies :only [meta-strategies]]))

(defn get-strat-regression
  [series strategy meta-strategy data x y strategy-regression]
  (case strategy-regression
        :linear
        (merge
         {:series series :strategy strategy :meta-strategy meta-strategy :reg-type "Linear"
          :xs (sel ($where {:Strategy strategy :MetaStrategy meta-strategy} data) :cols x)}
         (try (stats/linear-model
               (sel ($where {:Strategy strategy :MetaStrategy meta-strategy} data) :cols y)
               (sel ($where {:Strategy strategy :MetaStrategy meta-strategy} data) :cols x))
              (catch Exception e {})))))

(defn plot
  [data x y y-range regression strategy-regression]
  (with-data data
    (let [strats-with-data-noseries
          (filter (fn [{s :strategy ms :meta-strategy}]
                    (< 1 (nrow ($where {:Strategy s :MetaStrategy ms}))))
                  (for [s strategies ms meta-strategies] {:strategy s :meta-strategy ms}))
          strats-with-data
          (for [i (range (count strats-with-data-noseries))]
            {:series i
             :strategy (:strategy (nth strats-with-data-noseries i))
             :meta-strategy (:meta-strategy (nth strats-with-data-noseries i))})]
      (if (not-empty strats-with-data)
        (let [strat-regs
              (map (fn [{i :series s :strategy ms :meta-strategy}]
                     (if strategy-regression
                       (get-strat-regression i s ms data x y
                                             strategy-regression)))
                   strats-with-data)
              plot (clear-background
                    (scatter-plot x y :x-label (name x) :y-label (name y)
                                  :data
                                  ($where {:Strategy
                                           (:strategy (first strats-with-data))
                                           :MetaStrategy
                                           (:meta-strategy (first strats-with-data))})
                                  :legend true
                                  :series-label
                                  (format "%s+%s"
                                          (:strategy (first strats-with-data))
                                          (:meta-strategy (first strats-with-data)))))
              plot2 (reduce (fn [p {s :strategy ms :meta-strategy}]
                              (add-points p x y
                                          :data ($where {:Strategy s
                                                         :MetaStrategy ms})
                                          :series-label (format "%s+%s" s ms)))
                            plot (rest strats-with-data))
              plot3 (if y-range (apply set-y-range plot2 y-range) plot2)
              plot4 (if (nil? strategy-regression) plot3
                        (reduce
                         (fn [p strat-reg]
                           (if (or (nil? (:fitted strat-reg))
                                   (> 0.5 (:r-square strat-reg)))
                             p
                             (let [p2
                                   (add-lines p (:xs strat-reg) (:fitted strat-reg)
                                              :series-label
                                              (format "%s reg for %s+%s, r^2=%.2f"
                                                      (:reg-type strat-reg)
                                                      (:strategy strat-reg)
                                                      (:meta-strategy strat-reg)
                                                      (:r-square strat-reg)))
                                   n (.getRendererCount (.getPlot p2))
                                   renderer (-> p2 .getPlot (.getRenderer (dec n)))
                                   c (:color (get strategy-info (:strategy strat-reg)))]
                               (if (not= "none" (:meta-strategy strat-reg))
                                 (.setSeriesPaint renderer 0 (.darker c))
                                 (.setSeriesPaint renderer 0 c))
                               p2)))
                         plot3 strat-regs))
              plot-with-total-regression
              (cond (nil? regression)
                    plot4
                    (= regression :linear)
                    (let [lm (stats/linear-model (sel data :cols y)
                                                 (sel data :cols x))]
                      (if (> 0.5 (:r-square lm)) plot4
                          (add-lines plot3 (sel data :cols x) (:fitted lm)
                                     :series-label (format "Linear reg, r^2=%.2f"
                                                           (:r-square lm))))))]
          (dorun (map (fn [{i :series s :strategy ms :meta-strategy}]
                        (let [c (:color (get strategy-info s))
                              renderer (-> plot-with-total-regression
                                           .getPlot (.getRenderer i))]
                          (if (not= "none" ms)
                            (.setSeriesPaint renderer 0 (.darker c))
                            (.setSeriesPaint renderer 0 c))))
                      strats-with-data))
          plot-with-total-regression)))))

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
              (if-let [p (plot data (:x c) (:y c) (:y-range c)
                               (:regression c) (:strategy-regression c))]
                (save p
                 (format "%s/%s-%s--%s-%s.png" recordsdir
                         (:name problem) (:name c)
                         (name (:split-by c)) (str split))
                 :width 800 :height 600)))))
        (if-let [p (plot results (:x c) (:y c) (:y-range c)
                         (:regression c) (:strategy-regression c))]
          (save p
                (format "%s/%s-%s.png" recordsdir
                        (:name problem) (:name c))
                :width 800 :height 600))))))
