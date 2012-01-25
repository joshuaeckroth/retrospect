#^:shebang '[
exec java -Dfile.encoding=big5 -cp "$HOME/clojure/clojure-1.2.1/clojure.jar" clojure.main "$0" "$@"
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

(defn process-text [file encoding]
  (let [truedata (-> (slurp file :encoding encoding)
                     (str/trim)
                     (str/replace #"[\-/]" " ")
                     (str/replace #"[^\p{L}\s]" "")
                     (str/replace #"\s+" " ")
                     (str/lower-case))
        ;; consider only words with 1 or more letters
        truedata-large-words (filter #(>= (count %) 1) (str/split truedata #"\s+"))
        ;; split truedata into 50% for training and 50% for testing
        [training test] (split-at (int (/ (count truedata-large-words) 2))
                                  truedata-large-words)
        dictionary (sort (set training)) 
        ambiguous (apply str test)]
    [(apply str (interpose " " training)) (apply str (interpose " " test))
     dictionary ambiguous]))

(defn print-command-line-args
  []
  (println "Command-line args: [file] [encoding] [maxn]")
  (System/exit -1))

(if-not *command-line-args*
  (print-command-line-args)
  (if (not= 3 (count *command-line-args*)) (print-command-line-args)
      (let [[file encoding n] *command-line-args*
            [training test dictionary ambiguous] (process-text file encoding)
            maxn (Integer/parseInt n)
            models-csv (for [n (range 1 (inc maxn))]
                         (markov-model-to-csv (build-markov-model n training dictionary)))] 
        (spit "training.txt" training :encoding encoding)
        (spit "test.txt" test :encoding encoding)
        (spit "dictionary.txt" (apply str (interpose "\n" dictionary)) :encoding encoding)
        (spit "ambiguous.txt" ambiguous :encoding encoding)
        (doseq [n (range 1 (inc maxn))]
          (spit (format "model-%d.csv" n)
                (apply str (interpose "\n" (nth models-csv (dec n))))
                :encoding encoding)))))

