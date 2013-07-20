(ns retrospect.problems.aerial.hypotheses
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.state]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb 1.0 false nil [] "" "" {})])

(defn get-kb
  [accepted]
  (first (get accepted :kb)))

(defn update-kb
  [accepted unexplained hypotheses]
  [(get-kb accepted)])

(defn conflicts?
  [h1 h2]
  false)

(defn make-sensor-hyps
  [sensors time-prev time-now accepted hypotheses]
  (let [kb (get-kb accepted)
        objects (mapcat (fn [t] (sensed-at (first sensors) t))
                        (range time-prev (inc time-now)))]
    (map (fn [obj] (new-hyp "Obj" :object :object 1.0 true conflicts? []
                            (format "Object xc=%.2f yc=%.2f w=%.2f h=%.2f"
                                    (:xc obj) (:yc obj) (:w obj) (:h obj))
                            (format "Object xc=%.2f yc=%.2f w=%.2f h=%.2f"
                                    (:xc obj) (:yc obj) (:w obj) (:h obj))
                            obj))
         objects)))

(defn hypothesize
  [unexp accepted hypotheses time-now]
  [])
