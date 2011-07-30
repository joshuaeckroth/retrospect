(ns retrospect.problems.words.hypotheses
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]]))

(defn build-letter-index
  "Build a map indexed by letter, where each value is that letter's positions
   as a vector."
  [letters]
  (reduce (fn [index i] (update-in index [(nth letters i)] conj i))
          {} (range (count letters))))

(defn search-word
  "Find all possible occurrences (including jumps forward) of word in the index
   (built from build-letter-index). Each possible occurrence is a vector of
   positions (which are in increasing order, representing letter positions)."
  [index word]
  (loop [letters word
         incomplete []
         complete []]
    (if (empty? letters) complete
      (let [letter (first letters)
            newic (filter (fn [ic] (apply < ic))
                          (mapcat (fn [ic] (map #(conj ic %) (index letter)))
                                  incomplete))
            newic2 (if (not= letter (first word)) newic
                     (concat newic (for [i (index letter)] [i])))]
        (recur (rest letters) (filter #(not= (count word) (count %)) newic2)
               (concat complete (filter #(= (count word) (count %)) newic2)))))))

(defn search-dict-words
  [index dict]
  (reduce (fn [m w] (assoc m w (search-word index w))) {} dict))

(defn lookup-composite-prob2
  [models history back-n max-n composite]
  ;; get the appropriate model; as the composite grows,
  ;; get the bigger models (but max out at biggest model)
  (let [probs (map #(let [n (min max-n (+ back-n %))
                          model (get models n)
                          words-long (concat history (take % composite))
                          words (vec (take-last n words-long))]
                      ;; actually pull the prob from the model
                      (or (get model words) 0.0))
                   (range 1 (inc (count composite))))]
    (reduce * 1.0 probs)))

(defn conflicts?
  [hyp1 hyp2]
  (if (or (not= :words (:type hyp1)) (not= :words (:type hyp2))) nil
    (let [start1 (:start (:data hyp1))
          end1 (:end (:data hyp1))
          start2 (:start (:data hyp2))
          end2 (:end (:data hyp2))]
      (not (or (< end1 start2) (< end2 start1))))))

(defn lookup-prob
  [models words]
  (or (get (get models (count words)) words) 0.0))

(defn compute-apriori
  [models words pos-seq]
  (let [max-gap (apply max 1 (map #(apply - %) (partition 2 1 (reverse pos-seq))))]
    (* (if (= 1 max-gap) 1.0 (* 0.75 (dec max-gap)))
       (lookup-prob models words))))

(defn make-word-hyp
  [word pos-seq sensor-hyps models]
  (let [explains (map #(nth sensor-hyps %) pos-seq)]
    [(new-hyp "W" :words conflicts? (compute-apriori models [word] pos-seq)
              (format "Word: \"%s\" at positions %s\n\nExplains: %s"
                      word (str pos-seq)
                      (apply str (interpose ", " (map :id explains))))
              {:start (first pos-seq) :end (last pos-seq)})
     explains]))

(defn make-word-hyps
  [index dict sensor-hyps models]
  (let [words (reduce (fn [m w] (if (not-empty (m w)) m (dissoc m w)))
                      (search-dict-words index dict) dict)]
    (for [word (keys words) pos-seq (words word)]
      (make-word-hyp word pos-seq sensor-hyps models))))

(defn make-sensor-hyp
  [pos letter]
  (new-hyp "WS" :sensor nil 1.0 (format "Letter: '%c' at position %d" letter pos) {}))

(defn make-sensor-hyps
  [letters]
  (for [i (range (count letters))]
    (make-sensor-hyp i (nth letters i))))

(defn inconsistent
  [pdata hyps rejected]
  [])

(defn remove-prefix
  "Remove the common prefix a from the seq b; if there actually is
   no complete common prefix, return nil"
  [a b save-first?]
  (cond (empty? a) (vec b) 
        (empty? b) (if save-first? (vec a) [])
        (= (first a) (first b)) (recur (rest a) (rest b) save-first?)
        :else nil))



(defn build-composites
  [letters composite dict]
  (if (empty? letters) [composite]
    (let [cs (filter :remaining (map (fn [c] (let [r (remove-prefix (seq c) letters false)]
                                               {:word c :remaining r})) dict))]
      (mapcat (fn [{:keys [word remaining]}]
                (build-composites remaining (conj composite word) dict))
              cs))))

(defn non-zero-composites
  [letters models history history-conf max-n active-word dict]
  (first
    (drop-while
      empty?
      (for [offset (range (count letters))]
        (filter #(< 0.0 (:apriori %))
                (map (fn [composite]
                       {:offset offset :words composite
                        :apriori (lookup-composite-prob2
                                   models history history-conf max-n
                                   (if (= "" active-word) composite
                                     (concat [active-word] composite)))}) 
                     (build-composites (drop offset letters) [] dict)))))))

(defn make-hyp
  [composite active-word sens-hyps letters time-now]
  ;; this hyp explains as many sensor hyps (each representing a letter,
  ;; in order) as the composite covers, starting from an offset
  (let [words (:words composite) 
        offset (:offset composite)
        explains (take (count letters) (drop offset sens-hyps))
        predicted (remove-prefix (apply str words) (drop offset letters) true)]
    [(new-hyp "W" :words :shared-explains
              (:apriori composite)
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
        {:keys [dictionary left-off models]} (:problem-data ep-state)
        letters (map #(sensed-at sens %) (range left-off (inc time-now)))
        sensor-hyps (make-sensor-hyps letters)
        ep-sensor-hyps (reduce #(add-fact %1 %2 []) ep-state sensor-hyps)
        index (build-letter-index letters)
        hyps (make-word-hyps index dictionary sensor-hyps models)]
    (reduce #(apply add-hyp %1 %2) ep-sensor-hyps hyps)))

(defn hypothesize2
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
      (nil? letters)
      (assoc-in ep-sensor-hyps [:problem-data :prediction-met] false)
      ;; predictions met, no new information to process, so just update the
      ;; prediction (whatever's left)
      (empty? letters)
      (let [new-predicted (remove-prefix predicted s-letters true)]
        (-> ep-state
          (assoc-in [:problem-data :predicted] new-predicted)
          (assoc-in [:problem-data :predicted-start] (inc time-now))
          (assoc-in [:problem-data :prediction-met] true)))
      ;; otherwise, generate composite explainers, and indicate
      ;; that the prediction is no longer met (since there is something new to
      ;; explain)
      :else
      (let [dict (:dictionary (:problem-data ep-state))
            models (:models (:problem-data ep-state))
            history (:history (:problem-data ep-state))
            history-conf (:history-conf (:problem-data ep-state))
            composites (non-zero-composites
                         letters models history history-conf
                         (:MaxModelGrams params) active-word dict)
            hyps (map #(make-hyp % active-word sens-hyps letters time-now)
                      composites)
            ep (assoc-in ep-sensor-hyps [:problem-data :prediction-met] false)]
        (reduce #(apply add-hyp %1 %2) ep hyps)))))

(defn get-more-hyps
  [ep-state]
  nil)


(defn commit-decision
  [pdata accepted rejected time-now]
  (println accepted)
  pdata)

(defn commit-decision2
  [pdata accepted rejected time-now]
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
      ;; lose confidence in history, and drop prediction
      :else
      (assoc pdata :history-conf 0 :predicted [] :prediction-met false
             :predicted-start (inc time-now) :active-word ""))
    ;; accepted not empty
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
