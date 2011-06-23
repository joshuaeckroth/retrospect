(ns retrospect.problems.words.hypotheses
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]]))

(defn make-sensor-hyp
  [letter]
  (new-hyp "WS" :sensor nil 0.0 (format "Letter: %c" letter) {}))

(defn inconsistent
  [pdata hyps rejected]
  [])

(defn remove-prefix
  "Remove the common prefix a from the seq b; if there actually is
   no complete common prefix, return nil"
  [a b save-first?]
  (cond (empty? a) b
        (empty? b) (if save-first? a [])
        (= (first a) (first b)) (recur (rest a) (rest b) save-first?)
        :else nil))

(defn lookup-composite-prob
  [models history back-n max-n composite]
  ;; get the appropriate model; as the composite grows,
  ;; get the bigger models (but max out at biggest model)
  (let [probs (map #(let [n (min max-n (+ back-n %))
                          model (get models n)
                          words-long (concat history (take % composite))
                          words (vec (take-last n words-long))]
                      ;; actually pull the prob from the model
                      ;; (or 1e-9 if not in model)
                      (- (Math/log (or (get model words) 1.0e-20))))
                   (range 1 (inc (count composite))))]
    (reduce + 0.0 probs)))

(defn build-composites
  [letters composite dict]
  (if (empty? letters) [composite]
    (let [candidates (filter #(remove-prefix (seq %) letters false) dict)]
      (mapcat (fn [c] (build-composites
                                (remove-prefix c letters false)
                                (conj composite c) dict))
                      candidates))))

(defn make-hyp
  [models history history-conf max-n composite active-word
   sens-hyps letters time-now]
  ;; this hyp explains as many sensor hyps (each representing a letter,
  ;; in order) as the composite covers, starting from an offset
  (let [words (:words composite) 
        offset (:offset composite)
        explains (take (count letters) (drop offset sens-hyps))
        predicted (remove-prefix (apply str words) (drop offset letters) true)]
    [(new-hyp "W" :words :shared-explains
              (lookup-composite-prob
                models history history-conf max-n
                (if (= "" active-word) words 
                  (concat [active-word] words)))
              (format "The letters represent \"%s\"\nPredicting: %s\n\nExplains: %s"
                      (apply str (interpose " " words))
                      (apply str predicted)
                      (apply str (interpose ", " (map :id explains))))
              {:predicted predicted
               :predicted-start (inc time-now)
               :composite (if (= "" active-word) words
                            (concat [active-word] words))})
     explains]))

(defn hypothesize
  [ep-state sensors time-now params]
  (let [sens (first sensors) ;; only one sensor
        {:keys [predicted predicted-start active-word]} (:problem-data ep-state)
        s-letters (map #(sensed-at sens %)
                       (range predicted-start (inc time-now)))
        letters (remove-prefix predicted s-letters false)
        sens-hyps (map #(make-sensor-hyp %) (or letters s-letters)) 
        ep-sensor-hyps (reduce #(add-fact %1 %2 []) ep-state sens-hyps)]
    (cond
      ;; predictions not met (predicted was not a prefix of sensed-letters) so
      ;; just give back the ep-state with the sensor hyp but no explainers
      (nil? letters) ep-sensor-hyps
      ;; predictions met, no new information to process, so just update the
      ;; prediction (whatever's left)
      (empty? letters)
      (let [new-predicted (remove-prefix predicted s-letters true)]
        (-> ep-state
          (assoc-in [:problem-data :predicted] new-predicted)
          (assoc-in [:problem-data :predicted-start] (inc time-now))
          (assoc-in [:problem-data :prediction-met] true)))
      ;; otherwise, generate composite explainers
      :else
      (let [dict (:dictionary (:problem-data ep-state))
            models (:models (:problem-data ep-state))
            history (:history (:problem-data ep-state))
            history-conf (:history-conf (:problem-data ep-state))
            composites (first
                         (drop-while
                           empty? (for [offset (range (count letters))]
                                    (map (fn [composite] {:offset offset :words composite}) 
                                         (build-composites (drop offset letters) [] dict)))))]
        (reduce (fn [ep c]
                  (apply add-hyp ep
                         (make-hyp models history history-conf (:MaxModelGrams params)
                                   c active-word sens-hyps letters time-now))) 
                ep-sensor-hyps composites)))))

(defn get-more-hyps
  [ep-state]
  ep-state)

(defn commit-decision
  ([pdata accepted]
   (if (empty? accepted)
     (cond
       ;; prediction met and prediction finished; add active word to history
       (and (:prediction-met pdata) (empty? (:predicted pdata)))
       (-> pdata
         (update-in [:history] conj (:active-word pdata)) 
         (update-in [:history-conf] inc)
         (assoc :active-word "" :predicted [] :prediction-met false))
       ;; prediction met but prediction not finished; don't change anything
       (and (:prediction-met pdata) (not-empty (:predicted pdata)))
       pdata
       ;; no accepted hyps, but had a prediction (so prediction not met);
       ;; lose confidence in history, and drop prediction (keep old predicted-start),
       ;; so we can start over on the next input
       :else
       (assoc pdata :history-conf 0 :predicted [] :prediction-met false
              :active-word ""))
     (let [data (:data (first accepted))
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
              :history (if (empty? predicted) (vec (concat history composite)) 
                         (vec (concat history (butlast composite)))) 
              :history-conf (+ (:history-conf pdata)
                               (if (empty? predicted) (count composite)
                                 (dec (count composite)))) 
              :active-word (if (empty? predicted) "" (last composite))))))
  ([pdata accepted alts]
   (commit-decision pdata accepted)))
