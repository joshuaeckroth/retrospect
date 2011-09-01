(ns retrospect.problems.words.hypotheses
  (:import (misc LevenshteinDistance))
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.workspaces :only [new-hyp]])
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]]))

;; Sensor detections in the words domain come in the form of a string
;; of letters. These letters correspond to letters in the true words,
;; although the letters sensed may not be the true letters (depending
;; on the parameter :SensorNoise). Each sensed letter corresponds to a
;; single true letter (there are no additions or deletions).
;;
;; The task of the words domain hypothesizer is to offer hypotheses
;; about which words the letters represent (i.e. find word
;; boundaries). Finding the true words is somewhat ambiguous because
;; letter sequences like "looking" may come from the words "look in
;; g---" (for some word starting with "g") or "looking." The task is
;; much more ambiguous when :SensorNoise is positive, so the true
;; words "look in glass" may appear, for example, as "lokking" (with
;; only the "g" of "glass" being presented, as before). Since the
;; first "k" does not match any word, the hypothesizer may consider it
;; to be noise, and offer "locking" and "looking" and "look in g---"
;; and "lock in g---," etc. as alternative explanations of the letter
;; sequence.
;;
;; ## Algorithm
;;
;; A high-level overview of the hypothesizer follows. Assume the agent
;; has no beliefs resulting from past reasoning (i.e. the simulation
;; just started). The sensors are queried for all detections up to
;; "now." All possible word extractions are found, assuming no
;; noise. Each word is established as a hypothesis (that explains the
;; letters, i.e. sensor detections, that make up the word). The score
;; of each word (between 0.0 and 1.0) is a function of how closely the
;; word matches the sensor detections (in this case, the match is
;; perfect, since no noise is assumed) and the word's a-priori score
;; (from the unigram model of the text). Word hypotheses conflict with
;; other word hypotheses that explain the same sensor detections
;; (words that use the same letters from the same positions in the
;; stream).
;;
;; Then composite hypotheses are constructed from these word
;; hypotheses. A composite hypothesis is a sequence of words and
;; explains each of the word hypotheses. The word sequence must not
;; have any gaps in order to be a composite. Composite hypotheses are
;; scored by referring to the appropriate n-gram model (where n is the
;; length of the composite). Note that composite hypothesis scores do
;; not take into account how closely the words match the sensor
;; detections---the word hypotheses' scores already account for
;; that. Composite hypotheses conflict with other composite hypotheses
;; and word hypotheses that do not overlap (i.e. two composites
;; conflict if they cover some common part of the stream but not using
;; the same words, and a composite hypothesis and word hypothesis
;; conflict if the two hypotheses overlap in some part of the stream
;; and the composite hypothesis does not explain the word hypothesis,
;; meaning the word is not part of the composite).
;;
;; ## Boundary effects
;;
;; Typically, the hypothesizer is asked to produce hypotheses for a
;; sequence of letters (sensor detections) at a time when the sequence
;; of letters ends without completing a word. For example,
;; "lookinggla" likely comes from the true words "looking glass"
;; (which is a common phrase in /Through the looking glass/) but the
;; hypothesizer may only see the sequence "lookinggla." Presumably,
;; there is no other word that matches "gla" (and following "looking")
;; with a non-zero a-priori score, as read off the model. So the agent
;; would be find it reasonable to hypothesize, as a prediction, that
;; the next word is "glass." If, at the next reasoning cycle, the
;; prediction is not met, perhaps the agent should engage in
;; meta-reasoning.
;;
;; However, the code complexity for supporting predictions makes the
;; alternative more enticing: just don't hypothesize any explanations
;; for "gla" since no word matches it sufficiently well (especially
;; following "looking"). At the next reasoning cycle, "gla" will still
;; be unexplained, so it can be handled then, when more letters are
;; available.
;;
;; Both the predictive approach and the latter, adopted approach
;; mitigate the impact of boundary effects.

(def compute 0)
(def memory 0)

(defn build-letter-index
  "Build a map indexed by letter, where each value is that letter's positions
   as a vector."
  [letters]
  (reduce (fn [index i] (update-in index [(nth letters i)] conj i))
          {} (range (count letters))))

(defn search-word
  "Find all possible occurrences (including jumps forward) of word in the index
   (built from build-letter-index). Each possible occurrence is a
   vector of positions (which are in increasing order, representing
   letter positions)."
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

(defn conflicts?
  "Two words-domain hypotheses conflict if: (1) they are both composite
   hypotheses and it is not the case that the tail of one is the prefix of the
   other; or (2) they overlap in their start-end range."
  [hyp1 hyp2]
  (if (and (= :words (:type hyp1)) (= :words (:type hyp2)))
    (not-any? (fn [n] (or (= (take-last n (:pos-seqs (:data hyp1)))
                             (take n (:pos-seqs (:data hyp2))))
                          (= (take-last n (:pos-seqs (:data hyp2)))
                             (take n (:pos-seqs (:data hyp1))))))
              (range 1 (min (count (:pos-seqs (:data hyp1)))
                            (count (:pos-seqs (:data hyp2))))))
    (if (and (#{:words :single-word} (:type hyp1))
             (#{:words :single-word} (:type hyp2))) 
      (let [start1 (:start (:data hyp1))
            end1 (:end (:data hyp1))
            start2 (:start (:data hyp2))
            end2 (:end (:data hyp2))]
        (not (or (< end1 start2) (< end2 start1)))))))

(defn lookup-prob
  [models words]
  (get (get models (count words)) words))

(defn gap-sizes
  [starts-ends]
  (map #(- (second %) (first %))
       (partition 2 (interleave (map second (butlast starts-ends))
                                (map first (rest starts-ends))))))

(defn compute-apriori
  [models words starts-ends]
  (lookup-prob models words))

(defn make-word-hyp
  [word pos-seq left-off sensor-hyps models]
  (var-set (var compute) (inc compute))
  (let [explains (map #(nth sensor-hyps %) pos-seq)
        adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))]
    [(new-hyp "W" :single-word conflicts?
              (compute-apriori models [word] (map (fn [p] [p p]) adjusted-pos-seq))
              (format "Word: \"%s\" at positions %s"
                      word (str adjusted-pos-seq)
                      (apply str (interpose ", " (map :id explains))))
              {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
               :words [word] :pos-seqs [adjusted-pos-seq]})
     explains]))

(defn make-word-hyps
  [index left-off dict sensor-hyps models]
  (let [words (reduce (fn [m w] (if (not-empty (m w)) m (dissoc m w)))
                      (search-dict-words index dict) dict)]
    (filter
     (comp :apriori first)
     (for [word (keys words) pos-seq (words word)
           :when (= 1 (apply max 0 (gap-sizes (map (fn [p] [p p]) pos-seq))))]
       (make-word-hyp word pos-seq left-off sensor-hyps models)))))

(defn make-starts-ends
  [hyps]
  (map (fn [h] [(:start (:data h)) (:end (:data h))]) hyps))

(defn valid-composite?
  [hyps]
  (var-set (var compute) (inc compute))
  (let [min-gap (apply min (gap-sizes (make-starts-ends hyps)))]
    (and (> 2 min-gap) (< 0 min-gap)
         (not-any? (fn [[h1 h2]] (conflicts? h1 h2)) (partition 2 1 hyps)))))

(defn gen-valid-composites
  [word-hyps accepted max-n]
  (let [singular-word-hyps (map (fn [h] [h]) (concat word-hyps (take-last (dec max-n) accepted)))]
    (loop [i max-n
           composites []]
      (if (= 0 i) (filter #(< 1 (count %)) composites)
          (recur (dec i) (concat composites
                                 singular-word-hyps
                                 (filter valid-composite?
                                         (mapcat (fn [c] (map #(conj c %) word-hyps))
                                                 composites))))))))

(defn make-composite-hyps
  [models word-hyps accepted max-n]
  (let [composites (set (gen-valid-composites word-hyps accepted max-n))]
    (filter (comp :apriori first)
            (for [c composites]
              [(new-hyp "W" :words conflicts?
                        (compute-apriori models (mapcat (comp :words :data) c)
                                         (make-starts-ends c))
                        (format "Word sequence \"%s\" at positions %s"
                                (apply str (interpose " " (mapcat (comp :words :data) c)))
                                (apply str (interpose ", " (mapcat (comp :pos-seqs :data) c)))
                                (apply str (interpose ", " (map :id c))))
                        {:start (:start (:data (first c))) :end (:end (:data (last c)))
                         :pos-seqs (mapcat (comp :pos-seqs :data) c)
                         :words (mapcat (comp :words :data) c)})
               c]))))

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

(defn hypothesize
  [ep-state sensors time-now params]
  (binding [compute 0 memory 0]
    (let [sens (first sensors) ;; only one sensor
          {:keys [dictionary left-off accepted models]} (:problem-data ep-state)
          max-n (:MaxModelGrams params)
          letters (map #(sensed-at sens %) (range (inc left-off) (inc time-now)))
          sensor-hyps (make-sensor-hyps letters)
          ep-sensor-hyps (reduce #(add-fact %1 %2 []) ep-state sensor-hyps)
          index (build-letter-index letters)
          word-hyps (make-word-hyps index left-off dictionary sensor-hyps models)
          composite-hyps (make-composite-hyps models (map first word-hyps) accepted max-n)]
      [(reduce #(apply add-hyp %1 %2) ep-sensor-hyps (concat word-hyps composite-hyps))
       {:compute compute :memory memory}])))

(defn get-more-hyps
  [ep-state]
  nil)

(defn commit-decision
  [pdata accepted rejected time-now]
  (let [words-pos-seqs
        (sort-by (comp first second)
                 (partition 2 (mapcat (fn [hyp]
                                        (interleave (:words (:data hyp))
                                                    (:pos-seqs (:data hyp))))
                                      accepted))) 
        [words left-off]
        (loop [words []
               wps words-pos-seqs
               end-time (:left-off pdata)]
          (let [[word pos-seq] (first wps)]
            (cond (empty? wps) [words end-time]
                  (>= end-time (first pos-seq))
                  (recur words (rest wps) end-time)
                  :else (recur (conj words word)
                               (rest wps) (last pos-seq)))))]
    (-> pdata
        (update-in [:accepted] concat accepted)
        (update-in [:history] concat words)
        (assoc :left-off left-off))))

