(ns retrospect.problems.classify.truedata
  (:require [clojure.set :as set])
  (:require [clojure.string :as str])
  (:use [clojure.math.combinatorics :only [combinations]])
  (:use [granary.random])
  (:use [retrospect.state]))

(def stopwords
  #{"a" "able" "about" "across" "after" "all" "almost" "also" "am" "among"
    "an" "and" "any" "are" "as" "at" "be" "because" "been" "but" "by" "can"
    "cannot" "could" "dear" "did" "do" "does" "either" "else" "ever" "every"
    "for" "from" "get" "got" "had" "has" "have" "he" "her" "hers" "him" "his"
    "how" "however" "i" "if" "in" "into" "is" "it" "its" "just" "least" "let"
    "like" "likely" "may" "me" "might" "most" "must" "my" "neither" "no" "nor"
    "not" "of" "off" "often" "on" "only" "or" "other" "our" "own" "rather"
    "said" "say" "says" "she" "should" "since" "so" "some" "than" "that" "the"
    "their" "them" "then" "there" "these" "they" "this" "tis" "to" "too" "twas"
    "us" "wants" "was" "we" "were" "what" "when" "where" "which" "while" "who"
    "whom" "why" "will" "with" "would" "yet" "you" "your"})

(defn generate-truedata
  []
  (let [doccats (reduce (fn [m line] (let [[docid _ & cats] (str/split line #"\s+")]
                                 (assoc m docid (set cats))))
                   {} (str/split-lines (slurp (format "%s/classify/%s/listing.txt"
                                                 @datadir (:Dataset params)))))
        docwords (reduce (fn [m docid]
                      (let [docdata (slurp (format "%s/classify/%s/%s.txt"
                                              @datadir (:Dataset params) docid))]
                        (assoc m docid (set (filter #(not (stopwords %))
                                               (str/split docdata #"\s+"))))))
                    {} (keys doccats))
        [training-docids testing-docids] (split-at (int (* (/ (:Knowledge params) 100.0)
                                                           (count docwords)))
                                                   (my-shuffle (sort (keys docwords))))
        [word-doc-counts cat-probs cat-word-probs]
        (loop [docids training-docids
               word-doc-counts {}
               cat-probs {}
               cat-word-probs {}]
          (if (empty? docids)
            (let [new-cat-probs
                  (reduce (fn [m cat]
                       (assoc m cat
                              (/ (double (get m cat))
                                 (double (+ 2 (count training-docids))))))
                     cat-probs (keys cat-probs))
                  new-cat-word-probs
                  (reduce (fn [m [cat word]]
                       (assoc m [cat word]
                              (/ (double (get m [cat word]))
                                 (double (+ 2 (get word-doc-counts word))))))
                     cat-word-probs (keys cat-word-probs))]
              [word-doc-counts new-cat-probs new-cat-word-probs])
            (let [docid (first docids)
                  new-word-doc-counts
                  (reduce (fn [m word] (let [prior (get m word 1)]
                                   (assoc m word (inc prior))))
                     word-doc-counts (get docwords docid))
                  new-cat-probs
                  (reduce (fn [m cat] (let [prior (get m cat 1)]
                                  (assoc m cat (inc prior))))
                     cat-probs (get doccats docid))
                  new-cat-word-probs
                  (reduce (fn [m cat] (reduce (fn [m2 word] (let [prior (get m2 [cat word] 1)]
                                                  (assoc m2 [cat word] (inc prior))))
                                   m (get docwords docid)))
                     cat-word-probs (get doccats docid))]
              (recur (rest docids) new-word-doc-counts new-cat-probs new-cat-word-probs))))
        known-cats (keys cat-probs)
        word-prob-ranges (reduce (fn [m word]
                              (let [probs (sort
                                           (map (fn [cat] (get cat-word-probs [cat word] 0.5))
                                              known-cats))]
                                ;; [largest-prob, smallest-prob]
                                (assoc m word [(last probs) (first probs)])))
                            {} (keys word-doc-counts))]
    {:training {:test (zipmap (range (count training-docids))
                              (sort-by first (seq (select-keys docwords training-docids))))
                :cats (select-keys doccats training-docids)
                :known-cats known-cats
                :cat-probs cat-probs
                :cat-word-probs cat-word-probs
                :word-prob-ranges word-prob-ranges}
     :test (zipmap (range (count testing-docids))
                   (sort-by first (seq (select-keys docwords testing-docids))))
     :cats (select-keys doccats testing-docids)
     :known-cats known-cats}))
