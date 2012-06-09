(ns retrospect.problems.classify.truedata
  (:require [clojure.string :as str])
  (:use [retrospect.random])
  (:use [retrospect.state]))

(defn generate-truedata
  []
  (let [doccats (reduce (fn [m line] (let [[docid _ & cats] (str/split line #"\s+")]
                                 (assoc m docid cats)))
                   {} (str/split-lines (slurp (format "%s/classify/%s/listing.txt"
                                                 @datadir (:Dataset params)))))
        docwords (reduce (fn [m docid]
                      (let [docdata (slurp (format "%s/classify/%s/%s.txt"
                                              @datadir (:Dataset params) docid))]
                        (assoc m docid (set (str/split docdata #"\s+")))))
                    {} (keys doccats))
        [training-docids testing-docids] (split-at (int (* (/ (:Knowledge params) 100.0)
                                                           (count docwords)))
                                                   (my-shuffle (keys docwords)))
        known-cats (set (apply concat (vals doccats)))]
    {:training {:test (select-keys docwords training-docids)
                :cats (select-keys doccats training-docids)
                :known-cats known-cats}
     :test (select-keys docwords testing-docids)
     :cats (select-keys docwords training-docids)
     :known-cats known-cats}))
