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

;; http://rosettacode.org/wiki/Matrix_transposition#Clojure
(defn transpose [m] 
  (vec (apply map vector m)))

(def and-gate (fn [a b] (and a b)))

(def broken-and-gate (fn [a b] (not (and a b))))

(def or-gate (fn [a b] (or a b)))

(def broken-or-gate (fn [a b] (not (or a b))))

(def not-gate (fn [a] (not a)))

(def broken-not-gate (fn [a] a))

(def choices
  [{:fn true :str "true" :input true}
   {:fn false :str "false" :input true}
   {:fn and-gate :str "and" :input false :broken-fn broken-and-gate :broken false}
   {:fn or-gate :str "or" :input false :broken-fn broken-or-gate :broken false}
   {:fn not-gate :str "not" :input false :broken-fn broken-not-gate :broken false}])

(defn rand-gate
  [choices]
  (let [choice (rand-nth choices)
	broken? (< 0.8 (rand))]
    (assoc choice :broken broken?)))

(defn rand-gates
  [n choices]
  (let [mkgates #(map (fn [i] (rand-gate choices)) (range n))]
    (loop [gates (mkgates)]
      (let [input-count (count (filter :input gates))]
	;; ensure there are some inputs and fewer than (n-1) inputs
	(if (or (= input-count 0) (>= input-count (- n 1)))
	  (recur (mkgates))
	  gates)))))

(defn rand-wiring-column
  [i inputs gates]
  (if (not-any? #(= i %) inputs) ;; not an input column
    ;; make exactly "arity" number of trues
    (let [arity (arg-count (if (:broken (nth gates i))
			     (:broken-fn (nth gates i))
			     (:fn (nth gates i))))
	  trues (repeat arity true)
	  falses (repeat (- (count gates) arity) false)
	  column (vec (shuffle (concat trues falses)))]
      (if (nth column i) (recur i inputs gates) ;; try again if self-loop
	  column))
    
    ;; otherwise, it's an input column, so it's all falses
    (vec (repeat (count gates) false))))

(defn rand-wiring
  [n inputs gates]
  (transpose (for [i (range n)] (rand-wiring-column i inputs gates))))

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
	  (map (fn [i] (if (:input (nth gates i)) i))
	       (range (count gates)))))

(defn input-index
  [i gates]
  (let [inputs (find-inputs gates)]
    (loop [index 0]
      (cond (= index (count inputs)) nil
	    (= (nth inputs index) i) index
	    :else (recur (inc index))))))

(defn find-outputs
  [wiring]
  (filter identity
	  (map (fn [i] (if (not-any? identity (nth wiring i)) i))
	       (range (count wiring)))))

(defn output-index
  [i wiring]
  (let [outputs (find-outputs wiring)]
    (loop [index 0]
      (cond (= index (count outputs)) nil
	    (= (nth outputs index) i) index
	    :else (recur (inc index))))))

(defn valid-wiring
  [gates wiring inputs]
  (and
   ;; each input connects to at least on gate
   (every? identity (for [i inputs] (some identity (nth wiring i))))

   ;; no cycles
   (not (has-cycles wiring))))

(defn rand-gates-wiring
  []
  (let [n (+ 3 (rand-int 30))]
    (loop []
      (let [gates (rand-gates n choices)
	    inputs (find-inputs gates)
	    wiring (rand-wiring n inputs gates)]
	(cond
	 (nil? wiring) (recur)
	 
	 (valid-wiring gates wiring inputs)
	 [gates wiring]

	 :else (recur))))))

(defn find-gate-input-nodes
  [j wiring]
  (filter identity (map (fn [i] (if (nth (nth wiring i) j) i)) (range (count wiring)))))

(defn find-gate-input-node-values
  [i wiring outputs]
  (map (fn [j] (nth outputs j)) (find-gate-input-nodes i wiring)))

(defn eval-outputs
  [gates wiring outputs]
  (loop [i 0
	 os outputs]
    (if (= i (count gates)) os
	(let [input-node-values (find-gate-input-node-values i wiring os)
	      gate (nth gates i)]
	  (cond 
	  
	   (:input gate)
	   (recur (inc i) (assoc (vec os) i (:fn gate)))
	  
	   (and (= (nth os i) \?)
		(not-any? #(= \? %) input-node-values))
	   (recur (inc i) (assoc (vec os) i
				 (apply (if (:broken gate)
					  (:broken-fn gate)
					  (:fn gate))
					input-node-values)))

	   :else (recur (inc i) os))))))

(defn evaluate-circuit
  [gates wiring]
  (loop [outputs (repeat (count gates) \?)]
    (if (not-any? #(= \? %) outputs) outputs
	(recur (eval-outputs gates wiring outputs)))))

(defn format-evaluate-circuit
  [gates wiring]
  (let [outputs (evaluate-circuit gates wiring)
	input-idxs (find-inputs gates)
	output-idxs (find-outputs wiring)]
    (format "%s\n%s"
	    (apply str (interpose "; " (map #(format "input %d=%s"
						     (input-index % gates)
						     (nth outputs %))
					    input-idxs)))
	    (apply str (interpose "; " (map #(format "output %d=%s"
						     (output-index % wiring)
						     (nth outputs %))
					    output-idxs))))))

(defn make-graphviz-nodes
  [gates]
  (loop [i 0
	 graphviz ""]
    (if (= i (count gates)) graphviz
	(let [gate (nth gates i)]
	  (cond 

	   (:input gate)
	   (recur (inc i) (str graphviz
			       (format "gate%d [label=\"%d=%s\", shape=\"plaintext\"];\n"
				       i (input-index i gates) (:str gate))))

	   :else
	   (recur (inc i)
		  (str graphviz
		       (format "gate%d [label=\"%s\", color=\"%s\"];\n"
			       i (:str gate) (if (:broken gate) "blue" "black")))))))))

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
	 graphviz (apply str "digraph G {\n" (make-graphviz-nodes gates))]
    (cond (= i (count gates)) (str graphviz "}\n")

	  :else
	  (recur (inc i) (str graphviz (make-graphviz-edge i gates wiring))))))

(defn view-graphviz
  [filename gates wiring]
  (with-open [file (writer filename)]
    (.write file (make-graphviz-string gates wiring)))
  (sh "dot" "-Txlib" filename))




