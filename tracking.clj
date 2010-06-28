(ns org.artifice.thesis.simulation
  (:use clojure.test))

;; state functions

(defstruct state :grid :entities)

;; grid functions

;; top-left is 0,0; bottom right is (width-1),(height-1)

(defstruct grid :width :height :grid)

(defn new-grid [width height]
  (struct grid width height (vec (repeat height (vec (repeat width \space))))))

(defn pos-free [posx posy grid]
  (and (>= posx 0) (< posx (:width grid))
       (>= posy 0) (< posy (:height grid))
       (= \space (nth (nth (:grid grid) posx) posy))))

(deftest blank-grid-all-free
  (let [width 100 height 100
        grid (new-grid width height)]
    (is (every? (fn [[x y]] (pos-free x y grid))
                (for [x (range 100) y (range 100)] [x y])))))

;; entity functions

(defstruct entity :symbol :posx :posy :new?)

(defn symbol-used [symbol state]
  (some identity (map (fn [e] (= (:symbol e) symbol)) (:entities state))))

(defn pos-occupied [posx posy state]
  (some identity (map (fn [e] (and (= (:posx e) posx) (= (:posy e) posy))) (:entities state))))

(defn new-entity [state]
  (loop [symbol (char (+ 33 (rand-int 94)))
         posx (rand-int (:width (:grid state)))
         posy (rand-int (:height (:grid state)))]
    (if (or (symbol-used symbol state) (pos-occupied posx posy state))
      (recur (char (+ 33 (rand-int 94)))
             (rand-int (:width (:grid state)))
             (rand-int (:height (:grid state))))
      (struct-map entity :symbol symbol :posx posx :posy posy :new? true))))

(defn find-entity [entity state]
  (first (filter (fn [e] (and (= (:posx e) (:posx entity)) (= (:posy e) (:posy entity))))
                 (:entities state))))

(defn can-move? [dir posx posy grid]
  (cond (= dir "left")
        (if (pos-free (dec posx) posy grid)
          {:posx (dec posx) :posy posy} nil)
        (= dir "right")
        (if (pos-free (inc posx) posy grid)
          {:posx (inc posx) :posy posy} nil)
        (= dir "down")
        (if (pos-free posx (inc posy) grid)
          {:posx posx :posy (inc posy)} nil)
        (= dir "up")
        (if (pos-free posx (dec posy) grid)
          {:posx posx :posy (dec posy)} nil)))

(defn walk1 [e grid]
  (let [dir (nth ["left" "right" "down" "up"] (rand-int 4))
        newpos (can-move? dir (:posx e) (:posy e) grid)]
    (if newpos (assoc e :posx (:posx newpos) :posy (:posy newpos) :new? false)
        e)))

(defn update-grid [olde newe grid]
  (let [oldposx (:posx olde)
        oldposy (:posy olde)
        newposx (:posx newe)
        newposy (:posy newe)
        oldrow (assoc (nth (:grid grid) oldposy) oldposx \space)
        newgrid1 (assoc grid :grid (assoc (:grid grid) oldposy oldrow))
        newrow (assoc (nth (:grid newgrid1) newposy) newposx (:symbol newe))
        newgrid2 (assoc grid :grid
                       (assoc (assoc (:grid grid) oldposy oldrow)
                         newposy newrow))]
    newgrid2))
                         

;; sensor functions

(defstruct sensor :id :left :right :bottom :top :spotted)

(defn new-sensor [id left right bottom top]
  (struct-map sensor :id id :left left :right right :bottom bottom :top top :spotted []))

(defn update-spotted [s state]
  (assoc s :spotted
         (map (fn [e] {:posx (:posx e) :posy (:posy e)})
              (filter (fn [e] (not (= (:symbol e) \space)))
                      (for [posx (range (:left s) (inc (:right s)))
                            posy (range (:bottom s) (inc (:top s)))]
                        {:posx posx :posy posy :symbol (nth (nth (:grid (:grid state)) posy) posx)})))))


;; guess strategy functions

(defstruct strat-state-guess :entities)

(defn explain-new-entity [e strat-state state]
  (let [strat-s (assoc strat-state :entities (conj (:entities strat-state) {:posx (:posx e) :posy (:posy e)}))]
    (if (:new? (find-entity e state)) [1 1 strat-s] [1 0 strat-s])))

(defn explain-existing-entity [e n strat-state state]
  [1 0 strat-state])

(defn explain-guess [sensors strat-state state]
  (loop [entities (reduce concat (map :spotted sensors))
         decisions 0
         correct 0
         strat-s strat-state]
    (if (empty? entities) [decisions correct strat-s]
        (let [e (first entities)
              n (rand-int (inc (count (:entities strat-s))))
              [d c strat-s]
              (if (= n (count (:entities strat-state)))
                (explain-new-entity e strat-state state)
                (explain-existing-entity e n strat-state state))]
          (recur (rest entities) (+ d decisions) (+ c correct) strat-s)))))
  
;; strategy functions

(defn new-strat-state-guess []
  (struct-map strat-state-guess :entities []))

(defn new-strat-state [strategy]
  (cond (= strategy "guess") (new-strat-state-guess)))

(defn explain [strategy sensors strat-state state]
  (cond (= strategy "guess") (explain-guess sensors strat-state state)))

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
              (let [[decs cor strat-s-new] (explain strategy sens strat-s s)]
                (recur (inc i)
                       (map (fn [sen] (update-spotted sen s)) sens)
                       (+ decisions decs)
                       (+ correct cor)
                       strat-s-new
                       (loop [es (:entities s)
                              n 0
                              grid (:grid s)]
                         (if (< n (count es))
                           (let [olde (nth es n)
                                 newe (walk1 olde grid)
                                 newes (assoc es n newe)
                                 newgrid (update-grid olde newe grid)]
                             (recur newes (inc n) newgrid))
                           (assoc s :grid grid :entities es)))))
              [decisions correct])))]
    (struct-map result
      :steps steps :width width :height height :numes numes
      :numsens (count sensors) :senscoverage 0 :sensoverlap 0
      :strategy "guess" :decisions decisions :correct correct)))

