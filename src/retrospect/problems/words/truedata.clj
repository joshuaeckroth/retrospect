(ns retrospect.problems.words.truedata
  (:require [clojure.string :as str]))

(defn generate-truedata
  [datadir params]
  (let [truedata-txt (slurp (str datadir "/words/truedata.txt"))
        ambiguous (slurp (str datadir "/words/ambiguous.txt"))]
    (with-meta ambiguous {:words (str/split truedata-txt #" ")})))
