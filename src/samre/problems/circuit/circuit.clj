(ns samre.problems.circuit.circuit
  (:use [clojure.set])
  (:use [clojure.contrib.combinatorics :only (selections)]))

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

(def possible-inputs [true false])

(defn rand-input [] (rand-nth possible-inputs))

(def choices
  [{:fn rand-input :input true}
   {:fn and-gate :str "and" :input false :broken-fn broken-and-gate :broken false}
   {:fn or-gate :str "or" :input false :broken-fn broken-or-gate :broken false}
   {:fn not-gate :str "not" :input false :broken-fn broken-not-gate :broken false}])

(defn rand-gate
  [prob-broken choices]
  (let [choice (rand-nth choices)
	broken? (> (double (/ prob-broken 100)) (rand))]
    (assoc choice :broken broken?)))

(defn rand-gates
  [n prob-broken choices]
  (let [mkgates #(map (fn [i] (rand-gate prob-broken choices)) (range n))]
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

(defn find-broken-gates
  [gates]
  (filter identity (map #(if (and (not (:input (nth gates %)))
                                  (:broken (nth gates %))) %)
                        (range (count gates)))))

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

(defn make-input-vals
  [gates]
  (let [is (find-inputs gates)
        inputs (map (fn [i] (nth gates i)) is)]
    (apply hash-map (interleave is
                                (map (fn [{val-fn :fn}] (val-fn)) inputs)))))

(defn format-input-vals
  [input-vals gates]
  (format "[%s]" (apply str (interpose ", " (map #(format "%d=%s"
                                                          (input-index % gates)
                                                          (get input-vals %))
                                                 (keys input-vals))))))

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
  [params]
  (let [n (+ (:MinGates params) (rand-int (- (:MaxGates params) (:MinGates params))))]
    (loop []
      (let [gates (rand-gates n (:ProbBroken params) choices)
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

(defn find-all-gate-input-gates
  [j gates wiring]
  (let [inputs-to-j
        (set (filter identity (map (fn [i] (if (and (nth (nth wiring i) j)
                                                    (not (:input (nth gates i))))
                                             i)) (range (count wiring)))))]
    (apply union inputs-to-j
           (map #(find-all-gate-input-gates % gates wiring)
                inputs-to-j))))

(defn eval-outputs
  [gates wiring input-vals outputs eval-broken?]
  (loop [i 0
	 os outputs]
    (if (= i (count gates)) os
	(let [input-node-values (find-gate-input-node-values i wiring os)
	      gate (nth gates i)]
	  (cond 
	  
	   (:input gate)
	   (recur (inc i) (assoc (vec os) i (get input-vals i)))
	  
	   (and (= (nth os i) \?)
		(not-any? #(= \? %) input-node-values))
	   (recur (inc i) (assoc (vec os) i
				 (apply (if (and eval-broken? (:broken gate))
					  (:broken-fn gate)
					  (:fn gate))
					input-node-values)))

	   :else (recur (inc i) os))))))

(defn evaluate-circuit
  [gates wiring input-vals eval-broken?]
  (loop [outputs (repeat (count gates) \?)]
    (if (not-any? #(= \? %) outputs) outputs
        (recur (eval-outputs gates wiring input-vals outputs eval-broken?)))))

(defn format-evaluate-circuit
  [gates wiring input-vals eval-broken?]
  (let [outputs (evaluate-circuit gates wiring input-vals eval-broken?)
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

(defn find-differences
  [gates wiring input-vals]
  (let [expected (evaluate-circuit gates wiring input-vals false)
	observed (evaluate-circuit gates wiring input-vals true)]
    (filter identity (map (fn [i] (if (not= (nth expected i) (nth observed i))
                                    (let [oi (output-index i wiring)]
                                      (if oi [i oi (nth expected i) (nth observed i)]))))
			  (range (count expected))))))

(defn find-implicated-gates
  [gates wiring input-vals]
  (let [differences (find-differences gates wiring input-vals)
        output-gates (map (fn [[i _ _ _]] i) differences)]
    (map #(conj (find-all-gate-input-gates % gates wiring) %) output-gates)))

(defn generate-all-input-vals
  [gates]
  (let [inputs (find-inputs gates)
        sels (selections possible-inputs (count inputs))]
    (map (fn [sel] (apply hash-map (interleave inputs sel))) sels)))

(defn find-exhaustive-differences
  [gates wiring]
  (let [all-input-vals (generate-all-input-vals gates)]
    (map (fn [input-vals] (find-differences gates wiring input-vals)) all-input-vals)))

(defn find-undetectable-broken-gates
  [gates wiring]
  (let [all-input-vals (generate-all-input-vals gates)
        all-implicated-gates (map (fn [input-vals] (apply union (find-implicated-gates
                                                                 gates wiring input-vals)))
                                  all-input-vals)]
    (filter identity
            (for [i (range (count gates))]
              (if (and
                   (not (:input (nth gates i)))
                   (:broken (nth gates i))
                   (not (some (fn [implicated]
                                (some (fn [j] (= i j)) implicated))
                              all-implicated-gates)))
                i)))))