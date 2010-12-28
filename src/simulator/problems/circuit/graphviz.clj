(ns simulator.problems.circuit.graphviz
  (:use [clojure.java.io :only (writer)])
  (:use [clojure.contrib.shell :only (sh)])
  (:use [simulator.problems.circuit.circuit]))

(defn make-graphviz-nodes
  [gates]
  (loop [i 0
	 graphviz ""]
    (if (= i (count gates)) graphviz
	(let [gate (nth gates i)]
	  (cond 

	   (:input gate)
	   (recur (inc i) (str graphviz
			       (format "gate%d [label=\"%d\", shape=\"plaintext\"];\n"
				       i (input-index i gates))))

	   :else
	   (recur (inc i)
		  (str graphviz
		       (format "gate%d [label=\"%s.%d\", color=\"%s\", fontcolor=\"%s\"];\n"
			       i (:str gate) i
                               (if (:broken gate) "blue" "black")
                               (if (:broken gate) "blue" "black")))))))))

(defn make-graphviz-edge
  [i gates wiring]
  (let [con (filter identity (for [j (range (count gates))]
			       (if (nth (nth wiring i) j) j)))
	oi (output-index i wiring)]
    (if oi ;; gate connects to nothing; make an output node
      (format "gate%dout [label=\"%d\", shape=\"box\"];\ngate%d -> gate%dout;\n"
	      i oi i i)
      (apply str (map (fn [j] (format "gate%d -> gate%d;\n" i j)) con)))))

(defn make-graphviz-string
  [gates wiring]
  (loop [i 0
	 graphviz (apply str "digraph G {\n"
                         (make-graphviz-nodes gates))]
    (cond (= i (count gates)) (str graphviz "}\n")

	  :else
	  (recur (inc i) (str graphviz (make-graphviz-edge i gates wiring))))))

(defn view-graphviz
  [filename gates wiring]
  (with-open [file (writer filename)]
    (.write file (make-graphviz-string gates wiring)))
  (sh "dot" "-Txlib" filename))

(defn save-graphviz
  [dotfile pngfile gates wiring]
  (with-open [file (writer dotfile)]
    (.write file (make-graphviz-string gates wiring)))
  (sh "dot" "-Tpng" "-o" pngfile dotfile))

