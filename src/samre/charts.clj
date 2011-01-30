(ns samre.charts
  (:use [incanter.core])
  (:use [incanter.charts])
  (:use [incanter.io])
  (:use [incanter.stats :as stats]))

(defn get-regression
  [series meta-abduction data x y type]
  (let [xs (sel ($where {:MetaAbduction meta-abduction} data) :cols x)
        ys (sel ($where {:MetaAbduction meta-abduction} data) :cols y)]
    (case type
          :linear
          (merge
           {:series series :meta-abduction meta-abduction :reg-type "linear" :xs xs}
           (if (or (>= 3 (count xs)) (>= 3 (count ys))) {}
               (try
                 (stats/linear-model ys xs)
                 (catch Exception e {})))))))

(defn make-series-label
  [meta-abduction regression]
  (format "meta=%s%s" meta-abduction
          (cond (nil? regression) ""
                (nil? (:r-square regression)) ""
                (> 0.5 (:r-square regression)) ""
                :else
                (format ", %s reg r^2=%.2f"
                        (:reg-type regression)
                        (:r-square regression)))))

(defn plot
  [data x y y-range regression each-regression]
  (with-data data
    (let [plots-noseries
          (filter (fn [{meta-abduction :meta-abduction}]
                    (< 1 (nrow ($where {:MetaAbduction meta-abduction}))))
                  (for [meta-abduction ["true" "false"]] {:meta-abduction meta-abduction}))
          plots
          (for [i (range (count plots-noseries))]
            {:series i
             :meta-abduction (:meta-abduction (nth plots-noseries i))})]
      (if (not-empty plots)
        (let [each-regs
              (if (nil? each-regression) {}
                  (reduce (fn [m {i :series meta-abduction :meta-abduction}]
                            (assoc
                                m [meta-abduction] ;; the key of the map is a vector
                                (get-regression i meta-abduction data x y each-regression)))
                          {} plots))
              plot (clear-background
                    (scatter-plot x y :x-label (name x) :y-label (name y)
                                  :data
                                  ($where {:MetaAbduction (:meta-abduction (first plots))})
                                  :legend true
                                  :series-label
                                  (make-series-label
                                   (:meta-abduction (first plots))
                                   (each-regs [(:meta-abduction (first plots))]))))
              plot2 (reduce (fn [p {meta-abduction :meta-abduction}]
                              (add-points p x y
                                          :data ($where {:MetaAbduction meta-abduction})
                                          :series-label
                                          (make-series-label
                                           meta-abduction (each-regs [meta-abduction]))))
                            plot (rest plots))
              plot3 (if y-range (apply set-y-range plot2 y-range) plot2)
              plot4 (if (empty? each-regs) plot3
                        (reduce
                         (fn [p r]
                           (if (or (nil? (:fitted r))
                                   (> 0.5 (:r-square r)))
                             p
                             (let [p2
                                   (add-lines p (:xs r) (:fitted r))
                                   n (.getRendererCount (.getPlot p2))
                                   renderer (-> p2 .getPlot (.getRenderer (dec n)))]
                               (. renderer (setSeriesVisibleInLegend 0 false))
                               p2)))
                         plot3 (vals each-regs)))
              plot-with-total-regression
              (cond (nil? regression)
                    plot4
                    (= regression :linear)
                    (let [lm (stats/linear-model (sel data :cols y)
                                                 (sel data :cols x))]
                      (if (> 0.5 (:r-square lm)) plot4
                          (add-lines plot4 (sel data :cols x) (:fitted lm)
                                     :series-label (format "Linear reg, r^2=%.2f"
                                                           (:r-square lm))))))]
          plot-with-total-regression)))))

(defn read-results [filename]
  (read-dataset filename :header true))

(defn save-plots
  [recordsdir problem]
  (try
    (let [results (read-results (str recordsdir "/results-copy.csv"))]
      (doseq [c (:charts problem)]
        (if (:split-by c)
          (doseq [split (:split-list c)]
            (let [data ($where (merge (:filter c)
                                      {(:split-by c) {:$gt (- split (:split-delta c))
                                                      :$lt (+ split (:split-delta c))}})
                               results)
                  rs (sel results :cols (:x c))]
              (when (and (seq? rs) (< 1 (count rs)))
                (if-let [p (plot data (:x c) (:y c) (:y-range c)
                                 (:regression c) (:each-reg c))]
                  (save p
                        (format "%s/%s-%s--%s-%s.png" recordsdir
                                (:name problem) (:name c)
                                (name (:split-by c)) (str split))
                        :width 500 :height 500)))))
          (if-let [p (plot (if (:filter c) ($where (:filter c) results) results)
                           (:x c) (:y c) (:y-range c)
                           (:regression c) (:each-reg c))]
            (save p
                  (format "%s/%s-%s.png" recordsdir
                          (:name problem) (:name c))
                  :width 500 :height 500)))))
    (catch Exception e)))
