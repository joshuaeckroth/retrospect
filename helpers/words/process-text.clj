#^:shebang '[
exec java -Dfile.encoding=big5 -cp "$HOME/clojure/clojure-1.2.1/clojure.jar" clojure.main "$0" "$@"
]

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn build-markov-model
  "Build a Markov n-gram model of word transitions."
  [n training]
  (loop [tr (concat (repeat (dec n) "") ;; prepend n-1 blanks
                    (str/split training #" ")) 
         model {}]
    (if (> n (count tr)) model
        (let [words (take n tr)
              prior-this (or (get model words) 0)]
          (recur (rest tr)
                 (assoc model words (inc prior-this)))))))

(defn markov-model-to-csv
  [model]
  (for [words (keys model)]
    (format "%s,%d" (apply str (interpose "," words)) (get model words))))

(defn process-text [dataset]
  (let [[folder prefix] (str/split dataset "/")
        test (str/split-lines (slurp (format "%s/testing/%s_test.utf8" folder prefix)
                                     :encoding "utf-8"))
        truedata (-> (slurp (format "%s/training/") :encoding "utf-8")
                     (str/trim)
                     (str/replace #"[\-/]" " ")
                     (str/replace #"([^\p{L}\s])" " \1 ")
                     (str/replace #"\s+" " "))
        truedata-words (str/split truedata #"\s+")
        ;; split truedata into 90% for training and 10% for testing
        [training test] (split-at (int (* (count truedata-words) 0.9))
                                  truedata-words)
        dictionary (sort (set test)) 
        ambiguous (apply str test)]
    [(apply str (interpose " " training)) (apply str (interpose " " test))
     dictionary ambiguous]))

(defn print-command-line-args
  []
  (println "Command-line args: [file] [encoding] [maxn]")
  (System/exit -1))

(if-not *command-line-args*
  (print-command-line-args)
  (if (not= 2 (count *command-line-args*)) (print-command-line-args)
      (let [[dataset n] *command-line-args*
            [training test dictionary ambiguous] (process-text dataset)
            maxn (Integer/parseInt n)
            models-csv (for [n (range 1 (inc maxn))]
                         (markov-model-to-csv (build-markov-model n training)))]
        (spit "training.txt" training :encoding encoding)
        (spit "test.txt" test :encoding encoding)
        (spit "dictionary.txt" (apply str (interpose "\n" dictionary)) :encoding encoding)
        (spit "ambiguous.txt" ambiguous :encoding encoding)
        (doseq [n (range 1 (inc maxn))]
          (spit (format "model-%d.csv" n)
                (apply str (interpose "\n" (nth models-csv (dec n))))
                :encoding encoding)))))

