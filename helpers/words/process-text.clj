#^:shebang '[
exec java -cp "$HOME/.clojure/clojure.jar" clojure.main "$0" "$@"
]

(require '[clojure.string :as str])

(defn process-text [file]
  (let [truedata (-> (slurp file)
                   (str/replace #"[^a-zA-Z\s]" "")
                   (str/replace #"\s+" " ")
                   (str/lower-case)) 
        ;; get unique words, drop first
        ;; (the "word" consisting of a space)
        dictionary (rest (sort (set (str/split truedata #" ")))) 
        ambiguous (str/replace truedata #"\s" "")]
    [truedata dictionary ambiguous]))

(when *command-line-args*
  (let [[truedata dictionary ambiguous] (process-text (first *command-line-args*))] 
    (spit "truedata.txt" truedata)
    (spit "dictionary.txt" (apply str (interpose "\n" dictionary)))
    (spit "ambiguous.txt" ambiguous)))

