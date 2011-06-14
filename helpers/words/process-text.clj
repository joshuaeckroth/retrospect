#^:shebang '[
exec java -cp "$HOME/.clojure/clojure.jar" clojure.main "$0" "$@"
]

(require '[clojure.string :as str])

(defn build-markov-model
  "Build a Markov n-gram model of word transitions."
  [n truedata dictionary]
  (loop [td (str/split truedata #" ") 
         occur {}
         model {}]
    (if (> n (count td))
      ;; if done processing n-grams, divide out occurrences of each word
      ;; in order to get percentages
      (reduce (fn [m words]
                (assoc m words
                       (double (/ (get model words)
                                  (get occur (butlast words)))))) 
              {} (keys model))
      (let [words (take n td)
            prior-base (or (get occur (butlast words)) 0)
            prior-this (or (get model words) 0)]
        (recur (rest td)
               (assoc occur (butlast words) (inc prior-base))
               (assoc model words (inc prior-this)))))))

(defn markov-model-to-csv
  [model]
  (for [words (keys model)]
    (format "%s,%.10f" (apply str (interpose "," words)) (get model words))))

(defn process-text [file]
  (let [truedata (-> (slurp file)
                   (str/trim)
                   (str/replace #"[\-/]" " ")
                   (str/replace #"[^a-zA-Z\s]" "")
                   (str/replace #"\s+" " ")
                   (str/lower-case)) 
        dictionary (sort (set (str/split truedata #" "))) 
        ambiguous (str/replace truedata #"\s" "")]
    [truedata dictionary ambiguous]))

(when *command-line-args*
  (let [[truedata dictionary ambiguous] (process-text (first *command-line-args*))
        model-csv (markov-model-to-csv (build-markov-model 1 truedata dictionary))] 
    (spit "truedata.txt" truedata)
    (spit "dictionary.txt" (apply str (interpose "\n" dictionary)))
    (spit "ambiguous.txt" ambiguous)
    (spit "model.csv" (apply str (interpose "\n" model-csv)))))

