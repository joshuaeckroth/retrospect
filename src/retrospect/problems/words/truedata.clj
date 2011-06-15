(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str]))

(defn generate-truedata
  [datadir params]
  (let [truedata-txt (slurp (str datadir "/words/truedata.txt"))
        ambiguous (seq (slurp (str datadir "/words/ambiguous.txt")))]
    (with-meta (zipmap (range (count ambiguous)) ambiguous)
               {:words (str/split truedata-txt #" ")})))

