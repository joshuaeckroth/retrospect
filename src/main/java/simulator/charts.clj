(ns simulator.charts
  (:use [incanter.core])
  (:use [incanter.charts])
  (:use [incanter.io])
  (:use [incanter.stats :as stats])
  (:use [simulator.strategies :only (strategies)]))

(defn plot
  [data x y strategies y-range regression]
  (with-data data
    (let [plot (doto (scatter-plot x y :x-label (name x) :y-label (name y)
                                   :data ($where {:Strategy (first strategies)})
                                   :legend true :series-label (first strategies))
                 (clear-background))
          plot2 (reduce (fn [p strategy]
                          (add-points p x y
                                      :data ($where {:Strategy strategy})
                                      :series-label strategy))
                        plot (rest strategies))
          plot3 (if y-range (apply set-y-range plot2 y-range) plot2)]
      (cond (nil? regression)
            plot3
            (= regression :linear)
            (let [lm (stats/linear-model (sel data :cols y)
                                         (sel data :cols x))]
              (add-lines plot3 (sel data :cols x) (:fitted lm)
                         :series-label (format "Linear regression, r-square=%.2f"
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
              (save (plot data (:x c) (:y c) strategies (:y-range c) (:regression c))
                    (format "%s/%s-%s--%s-%s.png" recordsdir
                            (:name problem) (:name c)
                            (name (:split-by c)) (str split))))))
        (save (plot results (:x c) (:y c) strategies (:y-range c) (:regression c))
              (format "%s/%s-%s.png" recordsdir
                      (:name problem) (:name c)))))))


