(ns simulator.charts
  (:use [incanter.core])
  (:use [incanter.charts])
  (:use [incanter.io]))

(defn plot
  [data x y strategies sensor-coverage]
  (with-data data
    (let [plot (doto
		   (scatter-plot x y :x-label (name x) :y-label (name y) :legend true
				 :data ($where {:Strategy "guess"
						:SensorCoverage {:$gt (- sensor-coverage 5.0)
								 :$lt (+ sensor-coverage 5.0)}})
				 :series-label "guess"
				 :title (format "Sensor coverage: %.0f%%" sensor-coverage))
		 (set-y-range 0.0 100.0)
		 (clear-background))]
      (if (nil? (rest strategies)) plot
	  (reduce (fn [p strategy]
		    (add-points p x y
				:data ($where {:Strategy strategy
					       :SensorCoverage {:$gt (- sensor-coverage 5.0)
								:$lt (+ sensor-coverage 5.0)}})
				:series-label strategy))
		  plot (rest strategies))))))

(defn read-results [filename]
  (read-dataset filename :header true))

(defn save-plots
  [recordsdir]
  (let [results (read-results (str recordsdir "/results.csv"))]
    (save (plot results :NumberEntities :PercentCorrect ["guess" "nearest"] 100.0)
	  (str recordsdir "/numes-correct-100.png"))))


(comment
  (save-results (multiple-runs))
  (with-data (read-results)
    (view (scatter-plot :Walk-size :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (view (scatter-plot :Number-entities :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (view (scatter-plot :Steps :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (view (box-plot :Percent-correct :group-by :Strategy-index :legend true)))
  (with-data (read-results)
    (let [plot (scatter-plot :Steps :Milliseconds)]
      (view plot)
      (add-lines plot (sel (read-results) :cols 1)
		 (:fitted (stats/linear-model
			   (sel (read-results) :cols 0)
			   (sel (read-results) :cols 1))))))
  (with-data (sel (read-results) :filter #(= 50.0 (nth % 7)))
    (view (scatter-plot :Walk-size :Percent-correct :group-by :Strategy-index :legend true))))

