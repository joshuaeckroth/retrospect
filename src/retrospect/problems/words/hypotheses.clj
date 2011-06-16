(ns retrospect.problems.words.hypotheses
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]]))

(defn make-sensor-hyp
  [letter]
  (new-hyp "WS" :sensor nil 1.0 (format "Letter: %c" letter) {}))

(defn inconsistent
  [pdata hyps rejected]
  [])

(defn lookup-prob
  [model history word]
  (let [n (count (first (keys model))) 
        prefix (drop-last n history)
        prob (get model prefix)]
    (cond (< (count prefix) n) 1.0
          (nil? prob) 0.0
          :else prob)))

(defn lookup-composite-prob
  [model history composite] (rand (rand)))

(defn remove-prefix
  "Remove the common prefix a from the seq b; if there actually is
   no complete common prefix, return nil"
  [a b save-first?]
  (cond (empty? a) b
        (empty? b) (if save-first? a [])
        (= (first a) (first b)) (recur (rest a) (rest b) save-first?)
        :else nil))

(defn build-composites
  [letters composite dict]
  (if (empty? letters) [composite] 
    (let [candidates (filter #(remove-prefix (seq %) letters false) dict)]
      (concat (mapcat (fn [c] (build-composites
                                (remove-prefix c letters false)
                                (conj composite c) dict))
                      candidates)
              (map #(conj composite %) candidates)))))

(defn make-hyp
  [model history composite sens-hyps full-letters time-now]
  ;; this hyp explains as many sensor hyps (each representing a letter,
  ;; in order) as the composite covers
  (let [explains (take (- (count full-letters)
                          (count (remove-prefix (apply str composite)
                                                full-letters false))) 
                       sens-hyps)
        predicted (remove-prefix (apply str composite) full-letters true)]
    [(new-hyp "W" :words nil
              (lookup-composite-prob model history composite)
              (format "The letters represent \"%s\"\nPredicting: %s\n\nExplains: %s"
                      (apply str (interpose " " composite))
                      (apply str predicted)
                      (apply str (interpose ", " (map :id explains))))
              {:predicted predicted
               :predicted-start (inc time-now)
               :composite composite})
     explains]))

(defn hypothesize
  [ep-state sensors time-now params]
  (let [sens (first sensors) ;; only one sensor
        {:keys [predicted predicted-start]} (:problem-data ep-state)
        s-letters (map #(sensed-at sens %)
                       (range predicted-start (inc time-now)))
        letters (remove-prefix predicted s-letters false) 
        sens-hyps (map #(make-sensor-hyp %) s-letters) 
        ep-sensor-hyps (reduce #(add-fact %1 %2 []) ep-state sens-hyps)]
    (cond
      ;; predictions not met (predicted was not a prefix of sensed-letters) so
      ;; just give back the ep-state with the sensor hyp but no explainers
      (nil? letters) ep-sensor-hyps
      ;; predictions met, no new information to process, so just update the
      ;; prediction (whatever's left)
      (empty? letters)
      (-> ep-state
        (update-in [:problem-data :predicted] remove-prefix s-letters true)
        (assoc-in [:problem-data :predicted-start] (inc time-now))) 
      ;; otherwise, generate composite explainers
      :else
      (let [dict (:dictionary (:problem-data ep-state)) 
            full-letters (concat predicted letters)
            composites (build-composites full-letters [] dict)  
            model (:model (:problem-data ep-state))
            history (:history (:problem-data ep-state))]
        (reduce (fn [ep c]
                  (apply add-hyp ep
                         (make-hyp model history c sens-hyps
                                   full-letters time-now))) 
                ep-sensor-hyps composites)))))

(defn get-more-hyps
  [ep-state]
  ep-state)

(defn commit-decision
  ([pdata accepted]
   (if (empty? accepted) pdata
     (let [data (:data (first accepted)) ;; only one hyp is accepted
           {:keys [predicted predicted-start composite]} data
           history (:history pdata)]
       (assoc pdata
              :predicted predicted
              :predicted-start predicted-start
              ;; add to history; a composite hyp of 'n' words will possibly
              ;; predict the ending of the last word; if predicted is empty,
              ;; then the whole composite matches (and is added to the
              ;; history); otherwise, part of the last word is predicted, so
              ;; add all but the last word in the composite to the history
              :history (if (empty? predicted) (concat history composite)
                         (concat history (butlast composite)))))))
  ([pdata accepted alts]
   (commit-decision pdata accepted)))
