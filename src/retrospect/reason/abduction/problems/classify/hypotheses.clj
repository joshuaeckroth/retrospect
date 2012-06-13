(ns retrospect.reason.abduction.problems.classify.hypotheses
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.reason.abduction.workspace :only [new-hyp]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.profile :only [prof]]))

(defn generate-kb
  [training]
  [(new-hyp "KB" :kb :kb false (constantly false)
            [] [] "" "" {:known-cats (:known-cats training)})])

(defn get-kb
  [accepted lookup-hyp]
  (lookup-hyp (first (get accepted :kb))))

(defn make-sensor-hyps
  [sensor time-prev time-now accepted lookup-hyp]
  (prof :make-sensor-hyps
        (mapcat (fn [[docid words]]
                  (map (fn [word]
                       (new-hyp "Word" :word :word true nil
                                [] [] word (format "%s from %s" word docid)
                                {:docid docid :word word}))
                     words))
                (sensed-at sensor (inc time-prev)))))

(comment
  (defn conflicts?
    [hyp1 hyp2]))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (prof :hypothesize
        (let [kb (get-kb accepted lookup-hyp)
              word-hyps (sort-by :id (filter #(= :word (:type %)) forced-hyps))]
          (doall
           (mapcat (fn [cat]
                     (map (fn [word-hyp]
                          (new-hyp cat :category [cat (:word word-hyp)]
                                   false nil [word-hyp]
                                   [cat (disj (:known-cats kb) cat)]
                                   cat (format "%s is cat %s" (:docid word-hyp) cat)
                                   {:categories [cat] :word (:word word-hyp)
                                    :docid (:docid word-hyp)}))
                        word-hyps))
                   (sort (:known-cats kb)))))))
