(ns retrospect.problems.classify.truedata
  (:require [clojure.string :as str])
  (:use [clojure.contrib.combinatorics :only [combinations]])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn generate-truedata
  []
  (let [doccats (reduce (fn [m line] (let [[docid _ & cats] (str/split line #"\s+")]
                                 (assoc m docid (set cats))))
                   {} (str/split-lines (slurp (format "%s/classify/%s/listing.txt"
                                                 @datadir (:Dataset params)))))
        docwords (reduce (fn [m docid]
                      (let [docdata (slurp (format "%s/classify/%s/%s.txt"
                                              @datadir (:Dataset params) docid))]
                        (assoc m docid (set (str/split docdata #"\s+")))))
                    {} (keys doccats))
        [training-docids testing-docids] (split-at (int (* (/ (:Knowledge params) 100.0)
                                                           (count docwords)))
                                                   (my-shuffle (sort (keys docwords))))
        known-cats (set (apply concat (vals doccats)))
        catpaircounts (reduce (fn [m [_ cats]] (reduce (fn [m2 [cat1 cat2]]
                                              (let [prior (get m2 [cat1 cat2] 0)]
                                                (assoc m2 [cat1 cat2] (inc prior))))
                                            m (combinations cats 2)))
                         {} (seq doccats))
        catcounts (reduce (fn [m [_ cats]] (reduce (fn [m2 cat] (let [prior (get m2 cat 0)]
                                                      (assoc m2 cat (inc prior))))
                                        m cats))
                     {} (seq doccats))
        maxcatcount (apply max (vals catcounts))
        catgraph-relative-dot (reduce (fn [s [[cat1 cat2] count]]
                                   (format "%s%s -- %s [penwidth=%.2f, weight=%.2f]\n"
                                      s cat1 cat2
                                      (* 50.0 (double (/ count (+ (get catcounts cat1)
                                                                  (get catcounts cat2)))))
                                      (* 50.0 (double (/ count (+ (get catcounts cat1)
                                                                  (get catcounts cat2)))))))
                                 "" (seq catpaircounts))
        catgraph-dot (reduce (fn [s [[cat1 cat2] count]]
                          (format "%s%s -- %s [penwidth=%.2f, weight=%.2f]\n"
                             s cat1 cat2 (* 50.0 (double (/ count maxcatcount)))
                             (* 50.0 (double (/ count maxcatcount)))))
                        "" (seq catpaircounts))]
    (comment
      (spit (format "%s-cats-relative.dot" (:Dataset params))
            (format "graph g {\n%s\n}" catgraph-relative-dot))
      (spit (format "%s-cats.dot" (:Dataset params))
            (format "graph g {\n%s\n}" catgraph-dot)))
    {:training {:test (zipmap (range (count training-docids))
                              (sort-by first (seq (select-keys docwords training-docids))))
                :cats (select-keys doccats training-docids)
                :known-cats known-cats}
     :test (zipmap (range (count testing-docids))
                   (sort-by first (seq (select-keys docwords testing-docids))))
     :cats (select-keys doccats testing-docids)
     :known-cats known-cats}))
