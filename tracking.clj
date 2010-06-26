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

(defn draw-grid-ascii [grid]
  (println
   (for [row (:grid grid)]
     (println-str row))))

;; entity functions

(defstruct entity :symbol :posx :posy)

(defn new-entity [state]
  (loop [symbol (char (+ 33 (rand-int 94)))
         posx (rand-int (:width (:grid state)))
         posy (rand-int (:height (:grid state)))]
    (if (or (symbol-used symbol state) (pos-occupied posx posy state))
      (recur (char (+ 33 (rand-int 94)))
             (rand-int (:width (:grid state)))
             (rand-int (:height (:grid state))))
      (struct-map entity :symbol symbol :posx posx :posy posy))))

(defn symbol-used [symbol state]
  (some identity (map (fn [e] (= (:symbol e) symbol)) (:entities state))))

(defn pos-occupied [posx posy state]
  (some identity (map (fn [e] (and (= (:posx e) posx) (= (:posy e) posy))) (:entities state))))

(defn walk1 [e grid]
  (let [dir (nth ["left" "right" "down" "up"] (rand-int 4))
        newpos (can-move? dir (:posx e) (:posy e) grid)]
    (if newpos (assoc e :posx (:posx newpos) :posy (:posy newpos))
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

;; simulation functions

(defn run [steps numes]
  ; build up "numes" number of entities
  (let [state (loop [i 0
                     s (struct-map state :grid (new-grid 10 10) :entities [])]
                (if (< i numes)
                  (recur (inc i) (assoc s :entities (conj (:entities s) (new-entity s))))
                  s))]
    ; loop through steps
    (loop [i 0
           s state]
      (do
        (draw-grid-ascii (:grid s))
        (if (< i steps)
          (recur (inc i)
                 (loop [es (:entities s)
                        n 0
                        grid (:grid s)]
                   (if (< n (count es))
                     (let [olde (nth es n)
                           newe (walk1 olde grid)
                           newes (assoc es n newe)
                           newgrid (update-grid olde newe grid)]
                       (recur newes (inc n) newgrid))
                     (assoc s :grid grid :entities es))))
          s)))))


