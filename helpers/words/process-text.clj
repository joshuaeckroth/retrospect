#^:shebang '[
exec java -cp "$HOME/clojure/clojure-1.2.1/clojure.jar" clojure.main "$0" "$@"
]

(require '[clojure.string :as str])

(defn build-markov-model
  "Build a Markov n-gram model of word transitions."
  [n truedata dictionary]
  (loop [td (concat (repeat (dec n) "") ;; prepend n-1 blanks
                    (str/split truedata #" ")) 
         model {}]
    (if (> n (count td)) model
        (let [words (take n td)
              prior-this (or (get model words) 0)]
          (recur (rest td)
                 (assoc model words (inc prior-this)))))))

(defn markov-model-to-csv
  [model]
  (for [words (keys model)]
    (format "%s,%d" (apply str (interpose "," words)) (get model words))))

(defn process-text [file]
  (let [truedata (-> (slurp file)
                   (str/trim)
                   (str/replace #"[\-/]" " ")
                   (str/replace #"[^a-zA-Z\s]" "")
                   (str/replace #"\s+" " ")
                   (str/lower-case))
        ;; consider only words with 1 or more letters
        truedata-large-words (filter #(>= (count %) 1) (str/split truedata #"\s+"))
        dictionary (sort (set truedata-large-words)) 
        ambiguous (apply str truedata-large-words)]
    [(apply str (interpose " " truedata-large-words)) dictionary ambiguous]))

(when *command-line-args*
  (let [[truedata dictionary ambiguous] (process-text (first *command-line-args*))
        maxn (Integer/parseInt (second *command-line-args*))
        models-csv (for [n (range 1 (inc maxn))]
                     (markov-model-to-csv (build-markov-model n truedata dictionary)))] 
    (spit "truedata.txt" truedata)
    (spit "dictionary.txt" (apply str (interpose "\n" dictionary)))
    (spit "ambiguous.txt" ambiguous)
    (doseq [n (range 1 (inc maxn))]
      (spit (format "model-%d.csv" n) (apply str (interpose "\n" (nth models-csv (dec n))))))))

