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
        (let [kb (get-kb accepted lookup-hyp)]
          (concat
           (mapcat (fn [[docid words]]
                     (map (fn [word]
                          (new-hyp "Word" :word :word true nil
                                   [] [] word (format "%s from %s" word docid)
                                   {:docid docid :word word}))
                        words))
                   (sensed-at sensor (inc time-prev)))
           (map (fn [[cat1 cat2]]
                (new-hyp "CatPair" :catpair :catpair true nil
                         [] [] (format "%s,%s" cat1 cat2) (format "%s,%s" cat1 cat2)
                         {:cat1 cat1 :cat2 cat2}))
              (map sort (combinations (:known-cats kb) 2)))))))

(defn conflicts?
  [hyp1 hyp2]
  (or (some (set (:not-categories hyp1)) (:categories hyp2))
      (some (set (:not-categories hyp2)) (:categories hyp1))))

(defn update-kb
  [accepted unexplained hypotheses lookup-hyp]
  (map lookup-hyp (get accepted :kb)))

(defn hypothesize
  [forced-hyps accepted lookup-hyp]
  (prof :hypothesize
        (let [kb (get-kb accepted lookup-hyp)
              word-hyps (sort-by :id (filter #(= :word (:type %)) forced-hyps))
              docids (sort (set (map :docid word-hyps)))
              catpair-hyps (sort-by :id (filter #(= :catpair (:type %)) forced-hyps))]
          (doall
           (concat
            (mapcat (fn [cat]
                      (map (fn [word-hyp]
                           (new-hyp cat :category [cat (:word word-hyp)]
                                    false conflicts? [word-hyp] []
                                    cat (format "%s is cat %s" (:docid word-hyp) cat)
                                    {:categories [cat] :word (:word word-hyp)
                                     :docid (:docid word-hyp)}))
                         word-hyps))
                    (sort (:known-cats kb)))
            (mapcat
             (fn [docid]
               (mapcat (fn [catpair-hyp]
                         (let [{:keys [cat1 cat2]} catpair-hyp]
                           [(new-hyp (format "%s+%s" cat1 cat2) :catpair-both [cat1 cat2]
                                     false conflicts? [catpair-hyp] []
                                     (format "%s+%s" cat1 cat2) (format "%s+%s" cat1 cat2)
                                     {:categories [cat1 cat2]
                                      :docid docid})
                            (new-hyp (format "%s+!%s" cat1 cat2) :catpair-only-left [cat1 cat2]
                                     false conflicts? [catpair-hyp] []
                                     (format "%s+!%s" cat1 cat2) (format "%s+!%s" cat1 cat2)
                                     {:categories [cat1] :not-categories [cat2]
                                      :docid docid})
                            (new-hyp (format "!%s+%s" cat1 cat2) :catpair-only-right [cat1 cat2]
                                     false conflicts? [catpair-hyp] []
                                     (format "!%s+%s" cat1 cat2) (format "!%s+%s" cat1 cat2)
                                     {:categories [cat2] :not-categories [cat1]
                                      :docid docid})
                            (new-hyp (format "!%s+!%s" cat1 cat2) :catpair-neither [cat1 cat2]
                                     false conflicts? [catpair-hyp] []
                                     (format "!%s+!%s" cat1 cat2) (format "!%s+!%s" cat1 cat2)
                                     {:docid docid})]))
                       catpair-hyps))
             docids))))))
