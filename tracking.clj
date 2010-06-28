(ns org.artifice.thesis.simulation
  (:use clojure.test))

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

(defstruct entity :symbol :posx :posy :oposx :oposy :new?)

(defn symbol-used?
  "Check if a symbol has already been used by an existing entity."
  [symbol entities]
  (some identity (map (fn [e] (= (:symbol e) symbol)) entities)))

(defn new-entity
  "Create a new entity with a random symbol and random (free) location."
  [state]
  (loop [symbol (char (+ 33 (rand-int 94)))
         posx (rand-int (:width (:grid state)))
         posy (rand-int (:height (:grid state)))]
    (if (or (symbol-used? symbol (:entities state)) (not (pos-free? posx posy (:grid state))))
      (recur (char (+ 33 (rand-int 94)))
             (rand-int (:width (:grid state)))
             (rand-int (:height (:grid state))))
      (struct-map entity :symbol symbol :posx posx :posy posy :oposx nil :oposy nil :new? true))))

(defn find-entity
  "Retrieve an entity from a list of entities. The entity is found by checking posx,posy only."
  [entity entities]
  (first (filter (fn [e] (and (= (:posx e) (:posx entity)) (= (:posy e) (:posy entity))))
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
  [e grid]
  (let [dir (nth ["left" "right" "down" "up"] (rand-int 4))
        newpos (can-move? dir (:posx e) (:posy e) grid)]
    (if newpos (assoc e :posx (:posx newpos) :posy (:posy newpos)
                      :oposx (:posx e) :oposy (:posy e) :new? false)
        (assoc e :oposx (:posx e) :oposy (:posy e)))))

(defn update-grid
  "Redraw symbols on grid. Only affected row(s) and column(s) from one entity movement are altered."
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
  (struct-map sensor :id id :left left :right right :bottom bottom :top top :spotted []))

(defn update-spotted
  "Recreate 'spotted' vector for a sensor."
  [s grid]
  (assoc s :spotted
         (map (fn [e] {:posx (:posx e) :posy (:posy e)})
              (filter (fn [e] (not (= (:symbol e) \space)))
                      (for [posx (range (:left s) (inc (:right s)))
                            posy (range (:bottom s) (inc (:top s)))]
                        {:posx posx :posy posy :symbol (nth (nth (:grid grid) posy) posx)})))))


;; guess strategy functions

(defstruct strat-state-guess :entities)

(defn explain-new-entity
  "Update strategies state by posing e as a new entity. Explanation is correct if
   entity was actually new."
  [e strat-state]
  (let [strat-s (assoc strat-state :entities (conj (:entities strat-state) {:posx (:posx e) :posy (:posy e)}))]
    (if (:new? e) [1 1 strat-s] [1 0 strat-s])))

(defn explain-existing-entity
  "Update strategies state by posing e as a continuation of an existing entity.
   Explanation is correct if e actually was at existing entity's previous location
   in the previous step."
  [e n strat-state]
  (let [olde (nth (:entities strat-state) n)
        newe (assoc olde :posx (:posx e) :posy (:posy e))
        strat-s (assoc strat-state :entities (conj (remove (partial = olde) (:entities strat-state)) newe))]
    (if (and (= (:oposx e) (:posx olde)) (= (:oposy e) (:posy olde)))
      [1 1 strat-s]
      [1 0 strat-s])))

(defn explain-guess
  "Provide a 'guessed' explanation. Returns [decisions correct new-strat-state]."
  [sensors strat-state entities]
  (loop [es (reduce concat (map :spotted sensors))
         decisions 0
         correct 0
         strat-s strat-state]
    (if (empty? es) [decisions correct strat-s]
        (let [e (first entities)
              n (rand-int (count (:entities strat-s)))
              [d c new-strat-s]
              (if (= 0 (count (:entities strat-s)))
                (explain-new-entity (find-entity e entities) strat-s)
                (if (= (inc n) (count (:entities strat-s)))
                  (explain-new-entity (find-entity e entities) strat-s)
                  (explain-existing-entity e n strat-s)))]
          (recur (rest es) (+ d decisions) (+ c correct) new-strat-s)))))

(defn new-strat-state-guess []
  (struct-map strat-state-guess :entities []))

(defn new-strat-state [strategy]
  (cond (= strategy "guess") (new-strat-state-guess)))

(defn explain [strategy sensors strat-state entities]
  (cond (= strategy "guess") (explain-guess sensors strat-state entities)))

;; simulation functions

(defstruct result
  :steps :width :height :numes
  :numsens :senscoverage :sensoverlap
  :strategy :decisions :correct)

(defn run [steps numes width height strategy sensors]
  (let [[decisions correct]
                                        ; build up "numes" number of entities
        (let [state (loop [i 0
                           s (struct-map state :grid (new-grid width height) :entities [])]
                      (if (< i numes)
                        (recur (inc i) (assoc s :entities (conj (:entities s) (new-entity s))))
                        s))
              strat-state (new-strat-state strategy)]          
                                        ; loop through steps
          (loop [i 0
                 sens sensors
                 decisions 0
                 correct 0
                 strat-s strat-state
                 s state]
            (if (< i steps)
              (let [[decs cor strat-s-new] (explain strategy sens strat-s (:entities s))]
                (recur (inc i)
                       (map (fn [sen] (update-spotted sen (:grid s))) sens)
                       (+ decisions decs)
                       (+ correct cor)
                       strat-s-new
                       (loop [es (:entities s)
                              n 0
                              grid (:grid s)]
                         (if (< n (count es))
                           (let [newe (walk1 (nth es n) grid)
                                 newes (assoc es n newe)
                                 newgrid (update-grid newe grid)]
                             (recur newes (inc n) newgrid))
                           (assoc s :grid grid :entities es)))))
              [decisions correct])))]
    (struct-map result
      :steps steps :width width :height height :numes numes
      :numsens (count sensors) :senscoverage 0 :sensoverlap 0
      :strategy "guess" :decisions decisions :correct correct)))
