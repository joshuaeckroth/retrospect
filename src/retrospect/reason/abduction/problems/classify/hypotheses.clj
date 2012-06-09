(ns retrospect.reason.abduction.problems.classify.hypotheses
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb false (constantly false)
            [] [] "" "" {:known-cats (:known-cats training)})])

(defn make-sensor-hyps
  [sensor time-prev time-now hyps]
  (mapcat (fn [[docid words]]
          (map (fn [word]
               (new-hyp "Word" :word :word true (constantly false)
                        [] [] word (format "%s from %s" word docid)
                        {:docid docid :word word}))
             words))
        (sensed-at sensor (inc time-prev))))

(defn conflicts?
  [hyp1 hyp2]
  false)

(defn get-kb
  [accepted lookup-hyp]
  (lookup-hyp (first (get accepted :kb))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (let [kb (get-kb accepted lookup-hyp)]
    (mapcat (fn [cat]
              (map (fn [word-hyp]
                   (new-hyp cat :category [cat (:word word-hyp)] false conflicts?
                            [word-hyp] []
                            cat (format "%s is cat %s" (:docid word-hyp) cat)
                            {:cat cat :word (:word word-hyp) :docid (:docid word-hyp)}))
                 forced-hyps))
            (:known-cats kb))))
