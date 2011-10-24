(ns retrospect.problems.words.hypotheses
  (:import (misc LevenshteinDistance))
  (:require [clojure.set :as set])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.workspaces :only [find-unexplained new-hyp get-hyps]])
  (:use [retrospect.epistemicstates :only [add-hyp add-fact add-more-hyp]])
  (:use [retrospect.state]))

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

(def compute 0)
(def memory 0)

(defn lookup-prob
  [models words]
  (let [model (get models (count words))
        sum (reduce + 0 (map #(get model %) (filter #(= (butlast %) (butlast words)) (keys model))))
        c (get (get models (count words)) words)]
    ;; if we have a probability in the model, return it
    (if c (double (/ c sum))
        ;; otherwise, this sequence is not in the model;
        ;; if we are willing to learn, make the probability 0.2;
        ;; otherwise, it's 0.0
        (if (and (> 100 (:Knowledge params)) (:Learn params))
          0.2 0.0))))

(defn gap-sizes
  [starts-ends]
  (map #(- (second %) (first %))
       (partition 2 (interleave (butlast (map second starts-ends))
                                (rest (map first starts-ends))))))

(defn conflicts?
  "Two words-domain hypotheses conflict if: (1) they are both composite
   hypotheses and it is not the case that the tail of one is the prefix of the
   other; or (2) they overlap in their start-end range."
  [hyp1 hyp2]
  (if (and (= :words (:type hyp1)) (= :words (:type hyp2)))
    (some (fn [n] (or (= (take n (:pos-seqs (:data hyp1)))
                         (:pos-seqs (:data hyp2)))
                      (= (take-last n (:pos-seqs (:data hyp1)))
                         (:pos-seqs (:data hyp2)))
                      (= (take n (:pos-seqs (:data hyp2)))
                         (:pos-seqs (:data hyp1)))
                      (= (take-last n (:pos-seqs (:data hyp2)))
                         (:pos-seqs (:data hyp1)))))
          (range 1 (inc (min (count (:pos-seqs (:data hyp1)))
                             (count (:pos-seqs (:data hyp2)))))))
    (if (and (#{:words :single-word} (:type hyp1))
             (#{:words :single-word} (:type hyp2))) 
      (let [start1 (:start (:data hyp1))
            end1 (:end (:data hyp1))
            start2 (:start (:data hyp2))
            end2 (:end (:data hyp2))]
        (not (or (< end1 start2) (< end2 start1)))))))

(defn compute-apriori
  [models words sensor-noise]
  (if-let [prob (lookup-prob models words)]
    (/ prob (inc (Math/ceil (* sensor-noise (count words)))))))

(defn make-word-hyp
  [word pos-seq letters sensor-noise left-off sensor-hyps models]
  (let [explains (map #(nth sensor-hyps %) pos-seq)
        adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))]
    [(new-hyp "W" :single-word conflicts?
              (compute-apriori models [word] sensor-noise)
              (format "Word: \"%s\" at positions %s (%s) (sensor noise %.0f%%)"
                      word (str adjusted-pos-seq) (apply str letters) (* 100 sensor-noise))
              {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
               :words [word] :pos-seqs [adjusted-pos-seq]})
     explains]))

(defn make-learned-word-hyp
  [word pos-seq letters left-off sensor-hyps]
  (let [explains (map #(nth sensor-hyps %) pos-seq)
        adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))]
    [(new-hyp "W" :learned-word conflicts?
              0.2 ;; apriori (really apriori, not even computed!)
              (format "Learned word: \"%s\" at positions %s (%s)"
                      word (str adjusted-pos-seq) (apply str letters))
              {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
               :words [word] :pos-seqs [adjusted-pos-seq]})
     explains]))

(defn acceptable-noise?
  [letters word sensor-noise]
  (let [max-noise (Math/ceil (* sensor-noise (count word)))]
    (if (= max-noise 0) (= (apply str letters) (apply str word))
        (>= max-noise (count (filter #(not= (first %) (second %))
                                     (partition 2 (interleave letters word))))))))

(defn search-word
  "Find all possible occurrences (including jumps forward) of word in
   parts (partitions of the letters having same length as word). Each
   possible occurrence is a vector of positions (which are in
   increasing order, representing letter positions). Allow a maximum
   of max-noise letters to differ from word."
  [word parts sensor-noise]
  (filter (fn [[_ letters]] (acceptable-noise? word letters sensor-noise))
          (map (fn [lps] [(map first lps) (map second lps)]) parts)))

(defn search-dict-words
  [indexed-letters dict sensor-noise]
  ;; pre-compute all possible partitions (for all word lengths)
  (let [max-word-length (apply max 0 (map count dict))
        filter-gaps (fn [i-ls]
                      ;; create a starts-ends list like ([1 1] [2 2] ...)
                      (filter #(let [starts-ends (map (fn [i] [i i]) (map first %))]
                                 (= 1 (apply max 1 (gap-sizes starts-ends)))) i-ls))
        parts (map (fn [length] (filter-gaps (partition length 1 indexed-letters)))
                   (range 1 (inc max-word-length)))]
    (reduce (fn [m w] (assoc m w (search-word w (nth parts (dec (count w)))
                                              sensor-noise))) {} dict)))

(defn make-word-hyps
  [indexed-letters left-off dict sensor-noise sensor-hyps models]
  (let [words (reduce (fn [m w] (if (not-empty (m w)) m (dissoc m w)))
                      (search-dict-words indexed-letters dict sensor-noise) dict)]
    (filter
     (comp :apriori first)
     (for [word (keys words) [pos-seq letters] (get words word)]
       (make-word-hyp word pos-seq letters sensor-noise left-off sensor-hyps models)))))

(defn make-starts-ends
  [hyps]
  (map (fn [h] [(:start (:data h)) (:end (:data h))]) hyps))

(defn valid-composite?
  [hyps]
  (and (every? #(= 1 %) (gap-sizes (make-starts-ends hyps)))
       (not-any? (fn [[h1 h2]] (conflicts? h1 h2)) (partition 2 1 hyps))))

(defn gen-valid-composites
  [word-hyps accepted max-n]
  (let [composites (map #(sort-by (comp :start :data) %)
                        (mapcat (fn [n] (combinations (concat word-hyps accepted) n))
                                (range 2 (inc max-n))))]
    (filter valid-composite? composites)))

(defn make-composite-hyps
  [models word-hyps accepted max-n]
  (let [composites (set (gen-valid-composites word-hyps accepted max-n))]
    (filter (comp :apriori first)
            (for [c composites]
              [(new-hyp "W" :words conflicts?
                        (compute-apriori models (mapcat (comp :words :data) c) 0)
                        (format "Word sequence \"%s\" at positions %s"
                                (apply str (interpose " " (mapcat (comp :words :data) c)))
                                (apply str (interpose ", " (mapcat (comp :pos-seqs :data) c))))
                        {:start (:start (:data (first c))) :end (:end (:data (last c)))
                         :pos-seqs (mapcat (comp :pos-seqs :data) c)
                         :words (mapcat (comp :words :data) c)})
               c]))))

(defn make-sensor-hyp
  [pos letter]
  (new-hyp "WS" :sensor nil 1.0 (format "Letter: '%c' at position %d" letter pos)
           {:pos pos}))

(defn make-sensor-hyps
  [indexed-letters]
  (map (fn [[i letter]] (make-sensor-hyp i letter)) indexed-letters))

(defn inconsistent
  [pdata hyps rejected]
  [])

(defn make-dep-node
  [hyp]
  {:time (:start (:data hyp))
   :id (:id hyp)
   :str (apply str (:start (:data hyp)) ":" (interpose " " (:words (:data hyp))))})

(defn hypothesize
  [ep-state sensors time-now]
  (binding [compute 0 memory 0]
    (let [sens (first sensors) ;; only one sensor
          {:keys [dictionary left-off models]} (:problem-data ep-state)
          accepted (set (:accepted (:problem-data ep-state)))
          max-n (apply max (keys models))
          letters (map #(sensed-at sens %) (range (inc left-off) (inc time-now)))
          ;; if not enough sensed data to fill time, indexed-letters will have some
          ;; non-characters; so filter those out
          indexed-letters (filter #(= java.lang.Character (type (second %)))
                                  (map (fn [i] [i (nth letters i)]) (range (count letters))))
          sensor-hyps (make-sensor-hyps indexed-letters)
          ep-sensor-hyps (reduce #(add-fact %1 %2 []) ep-state sensor-hyps)
          ep-letters (assoc-in ep-sensor-hyps [:problem-data :indexed-letters] indexed-letters)
          word-hyps (make-word-hyps indexed-letters left-off dictionary 0.0 sensor-hyps models)
          composite-hyps (make-composite-hyps models (map first word-hyps) accepted max-n)]
      [(reduce (fn [ep [hyp explains]]
                 (add-hyp ep hyp explains (make-dep-node hyp)
                          (map make-dep-node (filter accepted explains))))
               ep-letters (concat word-hyps composite-hyps))
       {:compute compute :memory memory}])))

(defn get-more-hyps
  "The `(hypothesize)` function only offers word hypotheses that have
   no noise correction (no replaced characters; so the words must
   match identically with some part of the input stream). However,
   there may actually be noise in the sensors (i.e. the :SensorNoise
   parameter is positive), so it may be warranted to hypothesize noisy
   words (words with some letters replaced in order to match the input
   stream). We only want to hypothesize these noisy words
   in `(get-more-hyps)` since to do so in `(hypothesize)` would cause
   an unwanted (and unwarranted) proliferation of hypotheses.

   The difficulty in offering noisy word hypotheses is that some
   hypotheses have already been accepted, and we also want to generate
   composite hypotheses that are based on the offered noisy hypotheses
   and the already accepted, and not accepted, hypotheses found in the
   workspace of `ep-state`. "
  [ep-state]
  (cond (< 0 (:SensorNoise params))
        (let [{:keys [dictionary models left-off indexed-letters]} (:problem-data ep-state)
              max-n (apply max (keys models))
              sensor-noise (double (/ (:SensorNoise params) 100.0))
              ws (:workspace ep-state)
              existing-hyps (get-hyps ws)
              sensor-hyps (sort-by (comp :pos :data) (:forced ws))
              unexp-pos (map (comp :pos :data)
                             (set/intersection (find-unexplained ws) (:forced ws)))
              sub-indexed-letters (sort-by first (map (fn [i] (nth indexed-letters i)) unexp-pos))
              filter-existing (fn [hyps] (filter (fn [h] (not-any? #(= (:data (first h)) (:data %))
                                                                   existing-hyps))
                                                 hyps))
              word-hyps (filter-existing
                         (make-word-hyps sub-indexed-letters left-off
                                         dictionary sensor-noise sensor-hyps models))
              accepted (:accepted ws)
              composite-hyps (filter-existing
                              (make-composite-hyps models (map first word-hyps)
                                                   (filter #(= :single-word (:type %))
                                                           existing-hyps)
                                                   max-n))]
          (reduce (fn [ep [hyp explains]]
                    (add-more-hyp ep hyp explains (make-dep-node hyp)
                                  (map make-dep-node
                                       (filter existing-hyps
                                               (filter #(not= :sensor (:type %)) explains)))))
                  ep-state
                  (concat word-hyps composite-hyps)))
        (and (> 100 (:Knowledge params)) (:Learn params))
        (let [ws (:workspace ep-state)
              sensor-hyps (sort-by (comp :pos :data) (:forced ws))
              {:keys [indexed-letters dictionary left-off]} (:problem-data ep-state)
              unexp-pos (sort (map (comp :pos :data)
                                   (set/intersection (find-unexplained ws) (:forced ws))))
              contig-subsets (loop [ps (rest unexp-pos) subs [[(first unexp-pos)]]]
                               (cond (empty? ps) subs
                                     (= (first ps) (inc (last (last subs))))
                                     (recur (rest ps) (conj (vec (butlast subs))
                                                            (conj (vec (last subs)) (first ps))))
                                     :else (recur (rest ps) (conj subs [(first ps)]))))
              words (sort-by first (map (fn [subset] (map (fn [i] (nth indexed-letters i))
                                                          subset)) contig-subsets))
              new-words (filter (fn [w] (not (dictionary (apply str (map second w))))) words)
              hyps (map (fn [w] (make-learned-word-hyp (apply str (map second w))
                                                       (map first w) (map second w)
                                                       left-off sensor-hyps))
                        new-words)]
          (reduce (fn [ep [hyp explains]]
                    (add-more-hyp ep hyp explains (make-dep-node hyp) []))
                  ep-state hyps))))

(defn update-model
  [n model history words]
  (let [ws (concat (take-last (dec n) history) words)
        ngrams (partition n 1 ws)]
    (reduce (fn [m ngram] (if (get m ngram) (update-in m [ngram] inc) (assoc m ngram 1)))
            model ngrams)))

(defn commit-decision
  [pdata accepted rejected time-now]
  (let [words-pos-seqs
        (sort-by (comp first second)
                 (partition 2 (mapcat (fn [hyp] (interleave (:words (:data hyp))
                                                            (:pos-seqs (:data hyp)))) accepted))) 
        [words left-off]
        (loop [words []
               wps words-pos-seqs
               end-time (:left-off pdata)]
          (let [[word pos-seq] (first wps)]
            (cond (empty? wps) [words end-time]
                  (>= end-time (first pos-seq))
                  (recur words (rest wps) end-time)
                  :else (recur (conj words word)
                               (rest wps) (last pos-seq)))))
        learned-hyps (filter (fn [hyp] (= :learned-word (:type hyp))) accepted)
        learned-words (map (comp first :words :data) learned-hyps)
        models (:models pdata)
        history (:history pdata)
        max-n (:MaxModelGrams params)
        new-models (reduce (fn [ms i] (assoc ms i
                                             (update-model i (get models i)
                                                           (concat (repeat (dec max-n) "") history)
                                                           words)))
                           {} (range 1 (inc max-n)))]
    (-> pdata
        (update-in [:dictionary] set/union (set learned-words))
        (update-in [:accepted] concat accepted)
        (update-in [:history] concat words)
        (assoc :models new-models)
        (assoc :letters [])
        (assoc :left-off left-off))))

