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
              [] #_(map #(conj composite %) candidates)))))

(defn make-hyp
  [model history composite active-word sens-hyps diff-letters letters time-now]
  ;; this hyp explains as many sensor hyps (each representing a letter,
  ;; in order) as the composite covers
  (let [explains (take (+ diff-letters (count letters)) sens-hyps)
        predicted (remove-prefix (apply str composite) letters true)]
    [(new-hyp "W" :words nil
              (lookup-composite-prob model history composite)
              (format "The letters represent \"%s\"\nPredicting: %s\n\nExplains: %s"
                      (apply str (interpose " " composite))
                      (apply str predicted)
                      (apply str (interpose ", " (map :id explains))))
              {:predicted predicted
               :predicted-start (inc time-now)
               :composite (concat [active-word] composite)})
     explains]))

(defn hypothesize
  [ep-state sensors time-now params]
  (let [sens (first sensors) ;; only one sensor
        {:keys [predicted predicted-start active-word]} (:problem-data ep-state)
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
      (let [new-predicted (remove-prefix predicted s-letters true)
            new-active-word (if (empty? predicted) "" active-word)]
        (-> ep-state
          (assoc-in [:problem-data :predicted] new-predicted)
          (assoc-in [:problem-data :predicted-start] (inc time-now))
          (assoc-in [:problem-data :active-word] new-active-word))) 
      ;; otherwise, generate composite explainers
      :else
      (let [dict (:dictionary (:problem-data ep-state))
            composites (build-composites letters [] dict)
            model (:model (:problem-data ep-state))
            history (:history (:problem-data ep-state))
            diff-letters (- (count s-letters) (count letters))]
        (reduce (fn [ep c]
                  (apply add-hyp ep
                         (make-hyp model history c active-word sens-hyps
                                   diff-letters letters time-now))) 
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
                         (concat history (butlast composite))) 
              :active-word (if (empty? predicted) "" (last composite))))))
  ([pdata accepted alts]
   (commit-decision pdata accepted)))
