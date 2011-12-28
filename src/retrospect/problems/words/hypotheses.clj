(ns retrospect.problems.words.hypotheses
  (:import (misc LevenshteinDistance))
  (:require [clojure.set :as set])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.sensors :only [sensed-at]])
  (:use [retrospect.workspaces :only [find-unexplained new-hyp get-hyps]])
  (:use [retrospect.epistemicstates :only [add-hyp add-fact]])
  (:use [retrospect.problems.words.evaluate :only [hyps-equal?]])
  (:use [retrospect.problems.words.learning :only
         [update-features calc-centroid similarity]])
  (:use [retrospect.state]))

(def compute 0)
(def memory 0)

(defn conflicts?
  "Two words-domain hypotheses conflict if: (1) they are both composite
   hypotheses and it is not the case that the tail of one is the prefix of the
   other; or (2) they overlap in their start-end range."
  [hyp1 hyp2]
  (cond
   (or (= :sensor (:type hyp1)) (= :sensor (:type hyp2))) false
   (and (= :word-seq (:type hyp1)) (= :word-seq (:type hyp2)))
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
   :else
   (let [start1 (:start (:data hyp1))
         end1 (:end (:data hyp1))
         start2 (:start (:data hyp2))
         end2 (:end (:data hyp2))]
     (not (or (< end1 start2) (< end2 start1))))))

(defn count-changes
  [word letters]
  (reduce (fn [c i] (if (not= (nth word i) (nth letters i)) (inc c) c))
          0 (range (count word))))

(defn make-word-hyp
  [word pos-seq letters sensor-noise left-off sensor-hyps models]
  (let [explains (map #(nth sensor-hyps %) pos-seq)
        adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))
        unimodel (get models 1)
        similar-words (filter #(re-find (re-pattern (format ".*%s.*" (apply str word)))
                                        (first %))
                              (keys unimodel))
        prob (double (/ (get unimodel [word])
                        (reduce + (map (fn [w] (get unimodel w)) similar-words))))
        changes (count-changes word letters)
        ;; heuristic
        changes-factor (/ (- (count word) changes) (count word))
        apriori (max 0.001 (* prob changes-factor changes-factor))]
    (new-hyp "Word" :word :word conflicts?
             apriori :and explains []
             (format "Word: \"%s\" at positions %s (%s) (%d changes)"
                     word (str adjusted-pos-seq) (apply str letters) changes)
             {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
              :words [word] :pos-seqs [adjusted-pos-seq] :changes changes})))

(defn make-learned-word-hyp
  [word pos-seq letters left-off sensor-hyps avg-word-length centroid]
  (let [explains (map #(nth sensor-hyps %) pos-seq)
        adjusted-pos-seq (vec (map #(+ 1 left-off %) pos-seq))
        bk (- 1.0 (/ (:BelievedKnowledge params) 100.0))
        sim (Math/log (+ 1 (* 100 (similarity word centroid))))
        apriori (min 1.0 (* bk sim))]
    (new-hyp "WordLearn" :word :learned-word conflicts?
             apriori :and explains []
             (format "Learned word: \"%s\" at positions %s (%s) (sim: %.4f)"
                     word (str adjusted-pos-seq) (apply str letters) sim)
             {:start (first adjusted-pos-seq) :end (last adjusted-pos-seq)
              :words [word] :pos-seqs [adjusted-pos-seq] :sim sim})))

(defn acceptable-noise?
  [letters word sensor-noise]
  (let [max-noise (Math/ceil (* sensor-noise (count word)))]
    (if (= max-noise 0) (= (apply str letters) (apply str word))
        (>= max-noise (count (filter #(not= (first %) (second %))
                                     (partition 2 (interleave letters word))))))))

(defn make-starts-ends
  [hyps]
  (map (fn [h] [(:start (:data h)) (:end (:data h))]) hyps))

(defn gap-sizes
  [starts-ends]
  (map #(- (second %) (first %))
       (partition 2 (interleave (butlast (map second starts-ends))
                                (rest (map first starts-ends))))))

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
    (filter :apriori
            (for [word (keys words) [pos-seq letters] (get words word)]
              (make-word-hyp word pos-seq letters sensor-noise
                             left-off sensor-hyps models)))))

(defn valid-composite?
  [hyps]
  (and (every? #(= 1 %) (gap-sizes (make-starts-ends hyps)))
       (not-any? (fn [[h1 h2]] (conflicts? h1 h2)) (partition 2 1 hyps))))

(defn gen-valid-composites
  [word-hyps accepted max-n]
  (let [accepted-sorted (sort-by (comp :start :data) accepted)
        composites (map #(sort-by (comp :start :data) %)
                        (mapcat (fn [n]   
                                  (combinations
                                   (sort-by (comp :start :data)
                                            (concat (take-last (dec n) accepted-sorted)
                                                    word-hyps))
                                   n))
                                (range 2 (inc max-n))))]
    (set (filter valid-composite? (filter (fn [c] (some #(not (accepted %)) c))
                                          composites)))))

(defn lookup-prob
  [models word-hyps accepted]
  (let [acc-indexes (loop [indexes []
                           i 0
                           ws word-hyps]
                      (if (empty? ws) indexes
                          (let [c (count (:words (:data (first ws))))]
                            (recur 
                             (if (accepted (first ws))
                               (concat indexes (range i (+ i c)))
                               indexes)
                             (+ i c) (rest ws)))))
        words (vec (mapcat (comp :words :data) word-hyps))
        model (get models (count words))
        alt-ngrams (if (empty? acc-indexes)
                     (filter (fn [ws] (= (butlast ws) (butlast words))) (keys model))
                     (filter (fn [ws] (every? (fn [i] (= (nth ws i) (nth words i)))
                                              acc-indexes))
                             (keys model)))
        sum (reduce + (vals (select-keys model alt-ngrams)))
        c (get model words)]
    ;; if we have a probability in the model, return it
    (if c (max 0.001 (double (/ c (if (= 0 sum) (:sum (meta model)) sum))))
        ;; otherwise, this sequence is not in the model; if we have a
        ;; learned word hyp, the prob is the average of the word
        ;; probs; otherwise, prob is nil
        (if (some #{:learned-word} (map :type word-hyps))
          (let [s (reduce + (map :apriori word-hyps))]
            (max 0.001 (/ s (count word-hyps))))))))

(defn make-composite-hyps
  [models word-hyps accepted max-n]
  (let [composites (gen-valid-composites word-hyps accepted max-n)]
    (filter :apriori
            (for [c composites]
              (let [words (mapcat (comp :words :data) c)
                    pos-seqs (mapcat (comp :pos-seqs :data) c)
                    c-set (set c)
                    accepted-words (set/intersection accepted c-set)
                    unaccepted (set/difference c-set accepted)
                    depends (set (for [h accepted-words]
                                   (if-let [com (first (filter (fn [c] (some #{h} (:explains c)))
                                                               (filter #(= :word-seq (:type %))
                                                                       accepted)))]
                                     com h)))]
                (new-hyp "WordSeq" :word-seq :word-seq conflicts?
                         (lookup-prob models c accepted-words)
                         :and unaccepted depends ;; explains & depends
                         (format "Word sequence \"%s\" at positions %s"
                                 (apply str (interpose " " words))
                                 (apply str (interpose ", " pos-seqs)))
                         {:start (:start (:data (first c))) :end (:end (:data (last c)))
                          :pos-seqs pos-seqs :words words}))))))

(defn make-sensor-hyp
  [pos letter]
  (new-hyp "Sens" :sensor :sensor nil 1.0 nil [] []
           (format "Letter: '%c' at position %d" letter pos) {:pos pos}))

(defn make-sensor-hyps
  [indexed-letters]
  (map (fn [[i letter]] (make-sensor-hyp i letter)) indexed-letters))

(defn inconsistent
  [pdata hyps rejected]
  [])

(defn make-indexed-letters
  [letters]
  ;; if not enough sensed data to fill time, indexed-letters will have some
  ;; non-characters; so filter those out
  (filter #(= java.lang.Character (type (second %)))
          (map (fn [i] [i (nth letters i)]) (range (count letters)))))

(defn hypothesize
  [ep-state sensors time-now]
  (binding [compute 0 memory 0]
    (let [sens (first sensors) ;; only one sensor
          {:keys [dictionary left-off models]} (:problem-data ep-state)
          accepted (:accepted (:problem-data ep-state))
          max-n (apply max (keys models))
          letters (map #(sensed-at sens %) (range (inc left-off) time-now))
          indexed-letters (make-indexed-letters letters)
          sensor-hyps (make-sensor-hyps indexed-letters)
          ep-sensor-hyps (reduce #(add-fact %1 %2) ep-state sensor-hyps)
          ep-letters (assoc-in ep-sensor-hyps [:problem-data :indexed-letters]
                               indexed-letters)
          word-hyps (make-word-hyps indexed-letters left-off dictionary 0.0
                                    sensor-hyps models)
          composite-hyps (make-composite-hyps models word-hyps accepted max-n)]
      [(reduce (fn [ep hyp] (add-hyp ep hyp))
               ep-letters (concat word-hyps composite-hyps))
       {:compute compute :memory memory}])))

(defn make-sensor-noise-hyps
  [sub-indexed-letters left-off dictionary max-n
   sensor-noise sensor-hyps accepted models]
  (let [word-hyps (make-word-hyps sub-indexed-letters left-off
                                  dictionary sensor-noise sensor-hyps models)
        composite-hyps (make-composite-hyps models word-hyps accepted max-n)]
    (concat word-hyps composite-hyps)))

(defn make-learning-hyps
  [indexed-letters unexp-pos left-off dictionary sensor-hyps
   avg-word-length centroid last-time word-hyps]
  ;; some heuristics:
  ;; don't learn words that stop at or later than
  ;; 3 letters from the time boundary, or
  ;; whose 'sim' value is less than 1.0, or
  ;; do not fit the full length between some two word hyps
  ;; (if any word hyp proceeds or follows)
  (let [contig-subsets (loop [ps (rest unexp-pos)
                              subs (if-let [p (first unexp-pos)] [[p]] [])]
                         (cond (empty? ps) subs
                               (= (first ps) (inc (last (last subs))))
                               (recur (rest ps) (conj (vec (butlast subs))
                                                      (conj (vec (last subs))
                                                            (first ps))))
                               :else (recur (rest ps) (conj subs [(first ps)]))))
        contig-subsets-allsizes (mapcat (fn [sub]
                                          (mapcat #(partition % 1 sub)
                                                  (range (:MinLearnLength params)
                                                         (inc (:MaxLearnLength params)))))
                                        contig-subsets)
        words (sort-by first (map (fn [subset] (map (fn [i] (nth indexed-letters i))
                                                    subset)) contig-subsets-allsizes))
        ;; learn only words that are not in the dictionary
        new-words (filter (fn [w] (not (dictionary (apply str (map second w))))) words)
        snug? (fn [hyps h]
                (let [w-hyps-prior (filter #(< (:end (:data %)) (:start (:data h))) hyps)
                      w-hyps-after (filter #(> (:start (:data %)) (:end (:data h))) hyps)]
                  (and (or (empty? w-hyps-prior)
                           (some #(= (inc (:end (:data %))) (:start (:data h)))
                                 w-hyps-prior))
                       (or (empty? w-hyps-after)
                           (some #(= (dec (:start (:data %))) (:end (:data h)))
                                 w-hyps-after)))))
        learning-hyps (map (fn [w] (make-learned-word-hyp
                                    (apply str (map second w))
                                    (map first w) (map second w)
                                    left-off sensor-hyps avg-word-length centroid))
                           (filter #(< (first (last %)) (- last-time 3)) new-words))]
    (filter (fn [h] (snug? (concat word-hyps learning-hyps) h)) learning-hyps)))

(defn find-adjacent-hyps
  [unexp-pos word-hyps]
  (filter (fn [h] (let [s (:start (:data h))
                        e (:end (:data h))]
                    (not-empty
                     (set/intersection
                      unexp-pos (set [(inc s) (dec s)
                                      (inc e) (dec e)])))))
          word-hyps))

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
  (let [{:keys [dictionary models left-off centroid
                indexed-letters avg-word-length accepted]}
        (:problem-data ep-state)
        ws (:workspace ep-state)
        max-n (apply max (keys models))
        sensor-noise (double (/ (:SensorNoise params) 100.0))
        ;; need sensor-hyps to be a seq for make-learning-hyp function
        sensor-hyps (sort-by (comp :pos :data) (:forced ws))
        last-time (apply max 0 (map (comp :pos :data) sensor-hyps))
        word-hyps (filter #(= :word (:type %)) (get-hyps ws :static))
        word-seq-hyps (filter #(= :word-seq (:type %)) (get-hyps ws :static))
        existing-hyps (concat word-hyps word-seq-hyps)
        ;; unexplained positions
        unexp-pos (set (map (comp :pos :data)
                            (set/intersection (find-unexplained ws) (:forced ws))))
        ;; positions also from any nearby word-hyps
        word-hyp-positions (apply concat (mapcat (comp :pos-seqs :data)
                                                 (find-adjacent-hyps unexp-pos word-hyps)))
        positions (sort (set/union unexp-pos
                                   (set (if (= -1 left-off) word-hyp-positions
                                            (map #(- % left-off) word-hyp-positions)))))
        sub-indexed-letters (sort-by first (map (fn [i] (nth indexed-letters i))
                                                positions))
        sensor-noise-hyps (if (< 0 (:SensorNoise params))
                            (make-sensor-noise-hyps sub-indexed-letters left-off
                                                    dictionary max-n sensor-noise
                                                    sensor-hyps accepted models) [])
        learning-hyps (if (and (> 100 (:BelievedKnowledge params)) (:Learn params))
                        (make-learning-hyps indexed-letters positions left-off
                                            dictionary sensor-hyps
                                            avg-word-length centroid last-time
                                            (concat word-hyps sensor-noise-hyps)) [])
        composite-hyps (make-composite-hyps models (concat sensor-noise-hyps word-hyps)
                                            accepted max-n)]
    ;; don't add any already-existing hyps
    (reduce (fn [ep hyp] (add-hyp ep hyp))
            ep-state (filter (fn [h] (not-any? #(hyps-equal? % h) existing-hyps))
                             (concat sensor-noise-hyps learning-hyps composite-hyps)))))

(defn no-explainer-hyps
  [hyps pdata]
  [])

(defn update-model
  [n model history words]
  (let [ws (concat (take-last (dec n) history) words)
        ngrams (map vec (partition n 1 ws))
        new-model (reduce (fn [m ngram] (if (get m ngram)
                                          (update-in m [ngram] inc)
                                          (assoc m ngram 1)))
                          model ngrams)]
    (with-meta new-model {:sum (reduce + (vals new-model))})))

(defn commit-decision
  [pdata accepted _ _ _]
  (let [words-pos-seqs
        (sort-by (comp first second)
                 (partition 2 (mapcat (fn [hyp] (interleave (:words (:data hyp))
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
                               (rest wps) (last pos-seq)))))
        learned-hyps (filter (fn [hyp] (= :learned-word (:subtype hyp))) accepted)
        learned-words (map (comp first :words :data) learned-hyps)
        models (:models pdata)
        history (:history pdata)
        max-n (:MaxModelGrams params)
        new-models (reduce (fn [ms i]
                             (assoc ms i
                                    (update-model i (get models i)
                                                  (concat (repeat (dec max-n) "") history)
                                                  words)))
                           {} (range 1 (inc max-n)))
        new-dict (set/union (:dictionary pdata) (set learned-words))
        new-features (update-features (:features pdata) new-dict)
        new-centroid (calc-centroid new-features (get new-models 1))
        new-accepted (set (concat (:accepted pdata)
                                  (filter #(not= :sensor (:type %)) accepted)))]
    (-> pdata
        (update-in [:history] concat words)
        (assoc :accepted new-accepted)
        (assoc :models new-models)
        (assoc :dictionary new-dict)
        (assoc :avg-word-length (if (empty? new-dict) 0
                                    (double (/ (reduce + (map count new-dict))
                                               (count new-dict)))))
        (assoc :features new-features)
        (assoc :centroid new-centroid)
        (assoc :letters [])
        (assoc :left-off left-off))))

(defn retract
  [pdata hyp]
  pdata)
