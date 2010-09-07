(ns simulator.problems.circuit.core
  (:use [clojure.java.io :only (writer)])
  (:use [clojure.contrib.shell :only (sh)])
  (:use [clojure.set]))

;; http://stackoverflow.com/questions/1696693/
;; clojure-how-to-find-out-the-arity-of-function-at-runtime/1813967#1813967
(defn arg-count
  [f]
  (let [m (first (.getDeclaredMethods (class f)))
	p (.getParameterTypes m)]
    (alength p)))

(def and-gate (fn [a b] (and a b)))

(def or-gate (fn [a b] (or a b)))

(def not-gate (fn [a] (not a)))

(def choices [true false and-gate or-gate not-gate])

(def gates [true false and-gate or-gate not-gate])

(def outputs [true false \? \? \?])

(def wiring [[false false true true false]
	     [false false true false false]
	     [false false false true false]
	     [false false false false true]
	     [false false false false false]])

(defn rand-gates
  [n choices]
  (for [i (range n)]
    (let [r (rand-int (count choices))]
      (nth choices r))))

(defn rand-wiring
  [n inputs]
  (let [mkrow (fn [n]
		(for [j (range n)]
		  ;; if it's an input column, make it false
		  ;; because nothing connects to inputs
		  (if (some #(= j %) inputs) false
		      (= 0 (rand-int 2)))))]
    (for [i (range n)]
      (loop [row (mkrow n)]
	   ;; make sure an input row is not completely "false"
	(if (and (some #(= i %) inputs) (not-any? identity row))
	  (recur (mkrow n)) row)))))

(defn gates-point-to-inputs
  [inputs wiring]
  (some (fn [i] (some identity (for [j (range (count wiring))] (nth (nth wiring j) i))))
	inputs))

(defn arity-matches
  [gates wiring]
  (every? identity
	  (for [j (range (count gates))]
	    (or (or (= (nth gates j) true) (= (nth gates j) false))
		(= (arg-count (nth gates j))
		   (count (filter identity
				  (for [i (range (count gates))]
				    (nth (nth wiring i) j)))))))))

(defn has-cycle
  [wiring i visited]
  (let [neighbors (set (filter identity (for [j (range (count (nth wiring i)))]
					  (if (nth (nth wiring i) j) j))))
	unvisited (difference neighbors visited)]
    (cond
     (not-empty (intersection neighbors (conj visited i))) true
     (empty? unvisited) false
     :else (some identity (map (fn [neighbor] (has-cycle wiring neighbor (conj visited i)))
			       unvisited)))))

(defn has-cycles
  [wiring]
  (some identity (map (fn [i] (has-cycle wiring i #{})) (range (count wiring)))))

(defn find-inputs
  [gates]
  (filter identity
	  (map (fn [i] (if (or (= true (nth gates i))
			       (= false (nth gates i))) i))
	       (range (count gates)))))

(defn valid-wiring
  [gates wiring inputs]
  (cond (or (empty? inputs) (= (count gates) (count inputs))) false

	(gates-point-to-inputs inputs wiring) false

	(not (arity-matches gates wiring)) false

	(has-cycles wiring) false

	:else true))

(defn rand-gates-wiring
  []
  (let [n (rand-int 8)]
    (loop [c 0]
      (let [gates (rand-gates n choices)
	    inputs (find-inputs gates)
	    wiring (rand-wiring n inputs)]
	(if (valid-wiring gates wiring inputs) (do (println c) [gates wiring])
	    (recur (inc c)))))))

(defn make-graphviz-nodes
  [gates]
  (loop [i 0
	 graphviz ""]
    (cond (= i (count gates)) graphviz

	  (or (= (nth gates i) false) (= (nth gates i) true))
	  (recur (inc i) (str graphviz
			      (format "gate%d [label=\"%s\", shape=\"plaintext\"];\n"
				      i (nth gates i))))

	  :else
	  (recur (inc i)
	       (str graphviz
		    (format "gate%d [label=\"%s\"];\n"
			    i (cond (= (nth gates i) and-gate) "and"
				    (= (nth gates i) or-gate) "or"
				    (= (nth gates i) not-gate) "not"
				    :else "unknown")))))))

(defn make-graphviz-string
  [gates wiring]
  (loop [i 0
	 graphviz (apply str "digraph G {\n" (make-graphviz-nodes gates))]
    (cond (= i (count gates)) (str graphviz "}\n")

	  :else
	  (let [con (filter identity (for [j (range (count gates))]
				       (if (nth (nth wiring i) j) j)))
		con-str (apply str (map (fn [j] (format "gate%d -> gate%d;\n" i j))
					con))]
	    (recur (inc i) (str graphviz con-str))))))

(defn view-graphviz
  [filename gates wiring]
  (with-open [file (writer filename)]
    (.write file (make-graphviz-string gates wiring)))
  (sh "dot" "-Txlib" filename))




