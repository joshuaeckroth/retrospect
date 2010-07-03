(ns org.artifice.thesis.simulation
  (:use clojure.test)
  (:require [clojure.contrib.math :as math])
  (:use incanter.core)
  (:use incanter.charts))

;; state functions

(defstruct state :grid :entities)

;; grid functions

;; top-left is (0,0); bottom-right is (width-1,height-1)

(defstruct grid :width :height :grid)

(defn new-grid
  "Generate a width-by-height grid full of blanks."
  [width height]
  (struct grid width height (vec (repeat height (vec (repeat width \space))))))

(defn pos-free?
  "Check if a position in the grid is free (not occupied by an entity)."
  [posx posy grid]
  (and (>= posx 0) (< posx (:width grid))
       (>= posy 0) (< posy (:height grid))
       (= \space (nth (nth (:grid grid) posx) posy))))

(deftest blank-grid-all-free
  (let [width 100 height 100
        grid (new-grid width height)]
    (is (every? (fn [[x y]] (pos-free? x y grid))
                (for [x (range 100) y (range 100)] [x y])))))

;; entity functions

(defstruct entity :symbol :posx :posy :oposx :oposy :new? :created)

(defn symbol-used?
  "Check if a symbol has already been used by an existing entity."
  [symbol entities]
  (some identity (map (fn [e] (= (:symbol e) symbol)) entities)))

(defn new-entity
  "Create a new entity with a random symbol and random (free) location."
  [state step]
  (loop [symbol (char (+ 33 (rand-int 94)))
         posx (rand-int (:width (:grid state)))
         posy (rand-int (:height (:grid state)))]
    (if (or (symbol-used? symbol (:entities state))
	    (not (pos-free? posx posy (:grid state))))
      (recur (char (+ 33 (rand-int 94)))
             (rand-int (:width (:grid state)))
             (rand-int (:height (:grid state))))
      (struct-map entity :symbol symbol :posx posx :posy posy
		  :oposx nil :oposy nil :new? true :created step))))

(defn find-entity
  "Retrieve an entity from a list of entities. The entity is found by
   checking posx,posy only."
  [entity entities]
  (first (filter (fn [e] (and (= (:posx e) (:posx entity))
			      (= (:posy e) (:posy entity))))
                 entities)))

(defn can-move?
  "Determine if an entity can move one step in given direction."
  [dir posx posy grid]
  (cond (= dir "left")
        (if (pos-free? (dec posx) posy grid)
          {:posx (dec posx) :posy posy})
        (= dir "right")
        (if (pos-free? (inc posx) posy grid)
          {:posx (inc posx) :posy posy})
        (= dir "down")
        (if (pos-free? posx (inc posy) grid)
          {:posx posx :posy (inc posy)})
        (= dir "up")
        (if (pos-free? posx (dec posy) grid)
          {:posx posx :posy (dec posy)})))

(defn walk1
  "Move an entity one step in a random (free) direction."
  [e grid step]
  (let [dir (nth ["left" "right" "down" "up"] (rand-int 4))
        newpos (can-move? dir (:posx e) (:posy e) grid)]
    (if newpos (assoc e :posx (:posx newpos) :posy (:posy newpos)
                      :oposx (:posx e) :oposy (:posy e)
		      ; only switch ":new?" to false if current step is 2 or more
		      ; steps after this entity was new
		      :new?
		      (if (>= (:created e) (- step 4)) true false))
        (assoc e :oposx (:posx e) :oposy (:posy e)
	       :new? (if (>= (:created e) (- step 4)) true false)))))

(defn walkn
  "Move an entity n steps in a random walk."
  [n e grid step]
  (loop [i 0
	 ee e]
    (if (< i n)
      (recur (inc i) (walk1 ee grid step))
      ee)))

(defn update-grid
  "Redraw symbols on grid. Only affected row(s) and column(s)
   from one entity movement are altered."
  [e grid]
  (let [oldposx (:oposx e)
        oldposy (:oposy e)
        newposx (:posx e)
        newposy (:posy e)
        oldrow (assoc (nth (:grid grid) oldposy) oldposx \space)
        newgrid1 (assoc grid :grid (assoc (:grid grid) oldposy oldrow))
        newrow (assoc (nth (:grid newgrid1) newposy) newposx (:symbol e))
        newgrid2 (assoc grid :grid
                       (assoc (assoc (:grid grid) oldposy oldrow)
                         newposy newrow))]
    newgrid2))
                         

;; sensor functions

(defstruct sensor :id :left :right :bottom :top :spotted)

(defn new-sensor
  "Generate a new sensor with provided values and an empty 'spotted' vector."
  [id left right bottom top]
  (struct-map sensor :id id :left left :right right
	      :bottom bottom :top top :spotted []))

(defn update-spotted
  "Recreate 'spotted' vector for a sensor."
  [s grid]
  (assoc s :spotted
         (map (fn [e] {:posx (:posx e) :posy (:posy e)})
              (filter (fn [e] (not (= (:symbol e) \space)))
                      (for [posx (range (:left s) (inc (:right s)))
                            posy (range (:bottom s) (inc (:top s)))]
                        {:posx posx :posy posy
			 :symbol (nth (nth (:grid grid) posy) posx)})))))


;; guess strategy functions

(defn explain-new-entity
  "Update strategies state by posing e as a new entity. Explanation is correct if
   entity was actually new."
  [e entities strat-state]
  (let [strat-s (assoc strat-state
		  :entities (conj (:entities strat-state)
				  {:posx (:posx e) :posy (:posy e)
				   :step (:step strat-state)}))
	reale (find-entity e entities)]
    (if (:new? reale) [1 1 strat-s] [1 0 strat-s])))

(defn explain-existing-entity
  "Update strategies state by posing e as a continuation of an existing entity.
   Explanation is correct if e actually was at existing entity's previous location
   in the previous step."
  [e olde entities strat-state]
  (let [newe (assoc olde :posx (:posx e) :posy (:posy e) :step (:step strat-state))
        strat-s (assoc strat-state
		  :entities (conj (remove #(= olde %) (:entities strat-state)) newe))
	reale (find-entity newe entities)]
    (do (println "real: " reale) (println "old: " olde) (println "new: " newe)
	(if (and (= (:posx olde) (:oposx reale)) (= (:posy olde) (:oposy reale)))
	  [1 1 strat-s]
	  [1 0 strat-s]))))

(defn filter-old-entities
  "Filter only those entities that have a step less than the current step."
  [strat-state]
  (filter #(< (:step %) (:step strat-state)) (:entities strat-state)))

(defstruct strat-state-guess :step :entities)

(defn new-strat-state-guess []
  (struct-map strat-state-guess :step 0 :entities []))


; TODO: explain-guess and explain-nearest should return their explanation:
; explain as new or explain as existing; this is used by the (run) function to
; call another function that applies the explanation and checks its truth;
; thus, explain-guess and explain-nearest are not provided 'entities' or any
; other information about truth

(defn explain-guess
  "Provide a 'guessed' explanation. Returns [decisions correct new-strat-state]."
  [sensors strat-state entities]
  (loop [es (set (reduce concat (map :spotted sensors)))
         decisions 0
         correct 0
         strat-s (assoc strat-state :step (inc (:step strat-state)))]
    (if (empty? es) [decisions correct strat-s]
        (let [e (first es)
	      oldes (filter-old-entities strat-s)
              n (rand-int (count oldes))
              [d c new-strat-s]
              (if (= 0 (count oldes))
                (explain-new-entity e entities strat-s)
                (if (= (inc n) (count oldes))
                  (explain-new-entity e entities strat-s)
                  (explain-existing-entity e (nth oldes n) entities strat-s)))]
          (recur (rest es) (+ d decisions) (+ c correct) new-strat-s)))))

(defstruct strat-state-nearest :step :entities)

(defn new-strat-state-nearest []
  (struct-map strat-state-nearest :step 0 :entities []))

(defn find-nearest
  "Find entity nearest to e, and return [nearest-entity-index distance]."
  [e entities]
  (first (sort-by (fn [[ee dist]] dist)
                  (for [ee entities]
		    [ee (+ (math/abs (- (:posx e) (:posx ee)))
			   (math/abs (- (:posy e) (:posy ee))))]))))

; TODO: look at whole collection of sensor reports, and explain first the
; reports with lowest ambiguity (with 'unique nearest'); then remove the olde
; from the candidates and explain the next least ambiguous, etc.

(defn explain-nearest
  "Provide a 'nearest' explanation. If an existing entity is within 10 units
   of observed entity, then the existing entity (or a nearer one) is
   offered as the explanation. Returns [decisions correct new-strat-state]."
  [sensors strat-state entities]
  (loop [es (set (reduce concat (map :spotted sensors)))
         decisions 0
         correct 0
         strat-s (assoc strat-state :step (inc (:step strat-state)))]
    (if (empty? es) [decisions correct strat-s]
        (let [e (first es)
	      oldes (filter-old-entities strat-s)
              [d c new-strat-s]
              (if (= 0 (count oldes))
                (explain-new-entity e entities strat-s)
                (let [[ee dist] (find-nearest e oldes)]
                  (if (> dist 3)
		    (explain-new-entity e entities strat-s)
		    (explain-existing-entity e ee entities strat-s))))]
          (recur (rest es) (+ d decisions) (+ c correct) new-strat-s)))))

(defn new-strat-state [strategy]
  (cond (= strategy "guess") (new-strat-state-guess)
        (= strategy "nearest") (new-strat-state-nearest)))

(defn explain [strategy sensors strat-state entities]
  (cond (= strategy "guess") (explain-guess sensors strat-state entities)
        (= strategy "nearest") (explain-nearest sensors strat-state entities)))

;; simulation functions

(defstruct result
  :steps :width :height :numes
  :numsens :senscoverage :sensoverlap
  :strategy :decisions :correct :percent)

(defn run [steps numes walk width height strategy sensors]
  (let [[decisions correct] ; build up "numes" number of entities
        (let [state
	      (loop [i 0
		     s (struct-map state :grid (new-grid width height)
				   :entities [])]
		(if (< i numes)
		  (recur (inc i) (assoc s
				   :entities (conj (:entities s)
						   (new-entity s 0))))
		  s))
              strat-state (new-strat-state strategy)]
          (loop [i 0 ; loop through steps
                 sens (map (fn [sen] (update-spotted sen (:grid state))) sensors)
                 decisions 0
                 correct 0
                 strat-s strat-state
                 s state]
            (if (< i steps)
              (let [[decs cor strat-s-new]
		    (explain strategy sens strat-s (:entities s))
		    new-state (loop [es (:entities s)
				     n 0
				     grid (:grid s)]
				(if (< n (count es))
				  (let [newe (walkn walk (nth es n) grid i)
					newes (assoc es n newe)
					newgrid (update-grid newe grid)]
				    (recur newes (inc n) newgrid))
				  (assoc s :grid grid :entities es)))]
		(recur (inc i)
		       (map (fn [sen] (update-spotted sen (:grid new-state))) sens)
		       (+ decisions decs)
		       (+ correct cor)
		       strat-s-new
		       new-state))
	      [decisions correct])))]
    (struct-map result
      :steps steps :width width :height height :numes numes
      :numsens (count sensors) :senscoverage 0 :sensoverlap 0
      :strategy strategy :decisions decisions :correct correct
      :percent (float (/ correct decisions)))))


;; charting functions

(defn chart1 []
  (let [stepstart 490
	stepend 500
	width 10
	height 10
	numes 5
	walk 2
	sensors [(new-sensor "1" 0 9 0 9)]
	xs (range stepstart stepend)
	guess (doall (apply pcalls (for [steps xs]
				     #(:percent
				       (run steps numes walk width height
					    "guess" sensors)))))
	nearest (doall (apply pcalls (for [steps xs]
				       #(:percent
					 (run steps numes walk width height
					      "nearest" sensors)))))]
    (view (add-lines (xy-plot xs guess :series-label "Guesses"
			      :x-label "Steps" :y-label "% correct" :legend true)
		     xs nearest :series-label "Nearest"))))

(defn chart2 []
  (let [stepstart 20
	stepend 22
	width 10
	height 10
	numes [80]
	walks [1]
	sensors [(new-sensor "1" 0 9 0 9)]
	xs (range stepstart stepend)
	labels (for [n numes walk walks]
		 (format "Nearest es %d, walk %d" n walk))
	data (dataset
	      (conj labels "steps")
	      (for [steps xs]
		(conj (doall
;		       (apply pcalls 
			      (for [n numes walk walks]
				(do 
				  (println "steps: " steps ", numes: " n
					   ", walk: " walk)
				  (time (:percent (run steps n walk
						       width height
						       "nearest" sensors))))))
		      steps)))]
    (with-data data
      (view (reduce (fn [plot label] (add-lines plot ($ "steps") ($ label)
						:series-label label))
		    (xy-plot "steps" (first labels)
			     :series-label (first labels)
			     :x-label "Steps" :y-label "% correct" :legend true)
		    (rest labels))))))


; TODO: sentences: create a world state, then provide it to the sentence generator;
; the sentence generator reads the world state and chooses particular paths in the
; generator (following through a grammar) to correctly represent the state;
; generator chooses between 'it' and a specific reference every time it refers
; to an existing entity; sensor density is equivalent to saying more about the state
