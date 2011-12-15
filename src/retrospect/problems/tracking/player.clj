(ns retrospect.problems.tracking.player
  (:import (misc AlphanumComparator))
  (:import (java.awt Graphics2D Dimension GridBagLayout GridBagConstraints
                     BorderLayout RenderingHints BasicStroke Font Insets Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JSpinner SpinnerNumberModel))
  (:use [clojure.contrib.math :only [floor ceil]])
  (:use [clj-swing.label])
  (:use [clj-swing.frame])
  (:use [clj-swing.panel])
  (:use [retrospect.problems.tracking.sensors :only [sees]])
  (:use [retrospect.problems.tracking.movements :only
         [entities-at entity-movements]])
  (:use [retrospect.problems.tracking.truedata :only
         [format-movements-comparative]])
  (:use [retrospect.colors])
  (:use [retrospect.state]))

(def resized (atom nil))

(def diagram-width (ref nil))
(def diagram-height (ref nil))
(def grid-width (ref nil))
(def grid-height (ref nil))
(def grid-cell-width (ref nil))
(def grid-cell-height (ref nil))

(def percent-events-correct-label (label ""))
(def percent-events-wrong-label (label ""))
(def accuracy-label (label ""))
(def idcorrect-label (label ""))
(def unexp-label (label ""))
(def mouse-xy (label "Grid ?, ?"))

(defn draw-move
  [#^Graphics2D g oldx oldy newx newy color width]
  (let [oldpx (+ (* oldx @grid-cell-width) (/ @grid-cell-width 2))
	oldpy (+ (* oldy @grid-cell-height) (/ @grid-cell-height 2))
	newpx (+ (* newx @grid-cell-width) (/ @grid-cell-width 2))
	newpy (+ (* newy @grid-cell-height) (/ @grid-cell-height 2))]
    (doto g
      (.setColor (Color. (.getRed color) (.getGreen color) (.getBlue color) 150))
      (.setStroke (BasicStroke. width))
      (.drawLine oldpx oldpy newpx newpy))))

(defn draw-movements
  [#^Graphics2D g movs]
  (doseq [{:keys [x y ox oy time color]} movs]
    (let [degree (- @time-now time)
          width (double (+ 2 degree))]
      (draw-move g ox oy x y (var-color color degree) width))))

(defn fill-cell
  [#^Graphics2D g x y color]
  (doto g
    (.setColor color)
    (.fillRect (* x @grid-cell-width)
               (* y @grid-cell-height)
               @grid-cell-width @grid-cell-height)))

(defn fill-cell-entities
  [#^Graphics2D g x y es]
  (let [width (ceil (/ @grid-cell-width (count es)))]
    (doseq [i (range (count es))]
      (let [e (nth es i)
            movs (entity-movements @truedata e @time-prev (dec @time-now))
            left (+ (* i width) (* x @grid-cell-width))
            top (* y @grid-cell-height)]
        (doto g
          ;; draw block
          (.setColor (:color (first movs)))
          (.fillRect left top width @grid-cell-height)
          ;; draw id
          (.setColor white)
          (.setFont (.. g getFont (deriveFont Font/BOLD (float 12.0))))
          (.drawString (str e) (+ 3 left) (+ 15 top))
          ;; draw movement
          (draw-movements (filter :ot movs)))))))

(defn draw-grid
  [g]
  (dorun
   (for [x (range @grid-width) y (range @grid-height)]
     (doseq [sensor @sensors]
       (when (sees sensor x y)
         (if (:sees-color (meta sensor))
           (fill-cell g x y yellow-alpha)
           (fill-cell g x y gray-alpha))))))
  (when (and @truedata (> @time-now 0))    
    (dorun
     (for [x (range @grid-width) y (range @grid-height)]
       (let [es (sort-by str (AlphanumComparator.)
                         (entities-at @truedata x y (dec @time-now)))]
         (when (not-empty es)
           (fill-cell-entities g x y es)))))))

(defn render
  [g]
  (when @resized
    (dosync
     (alter diagram-width (constantly (.. g (getClipBounds) width)))
     (alter diagram-height (constantly (.. g (getClipBounds) height)))
     (alter grid-width (constantly (:GridWidth params)))
     (alter grid-height (constantly (:GridHeight params)))
     (alter grid-cell-width (constantly (floor (/ @diagram-width @grid-width))))
     (alter grid-cell-height (constantly (floor (/ @diagram-height @grid-height)))))
    (swap! resized (constantly nil)))
  (let [img (new BufferedImage @diagram-width @diagram-height
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setRenderingHint (. RenderingHints KEY_ANTIALIASING)
                         (. RenderingHints VALUE_ANTIALIAS_ON))
      (.setColor white)
      (.fillRect 0 0 @diagram-width @diagram-height))
    (draw-grid bg)
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(def player-diagram
  (doto (proxy [JPanel] []
          (paint [g] (render g)))
    (.addMouseListener
     (proxy [java.awt.event.MouseListener] []
       (mouseClicked [e])
       (mouseEntered [e])
       (mouseExited [e]
         (. mouse-xy setText "Grid ?, ?"))
       (mousePressed [e])
       (mouseReleased [e])))
    (.addMouseMotionListener
     (proxy [java.awt.event.MouseMotionListener] []
       (mouseDragged [e])
       (mouseMoved [e]
         (. mouse-xy setText
            (let [x (floor (/ (.getX e) @grid-cell-width))
                  y (floor (/ (.getY e) @grid-cell-height))]
              (format "Grid %d, %d" x y))))))
    (.addComponentListener
     (proxy [java.awt.event.ComponentListener] []
       (componentHidden [_])
       (componentMoved [_])
       (componentResized [_] (swap! resized (constantly true)))
       (componentShown [_])))))

(defn player-update-diagram
  []
  (.repaint player-diagram))

(defn player-setup-diagram
  [p]
  (doto p
    (.setLayout (BorderLayout.))
    (.add player-diagram)))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 0 5 0)
          _ (label "PEC:")
          :gridx 1
          _ percent-events-correct-label
          :gridx 0 :gridy 1
          _ (label "PEW:")
          :gridx 1
          _ percent-events-wrong-label
          :gridx 0 :gridy 2
          _ (label "Accuracy:")
          :gridx 1
          _ accuracy-label
          :gridx 0 :gridy 3
          _ (label "IDCorrect:")
          :gridx 1
          _ idcorrect-label
          :gridx 0 :gridy 4
          _ (label "Unexplained:")
          :gridx 1
          _ unexp-label
          :gridx 0 :gridy 5
          _ mouse-xy]))

(defn player-update-stats
  []
  (if (> @time-now 0)
    (let [t (int (/ (dec @time-now) (:StepsBetween params)))
          results (get (:results @or-state) t)]
      (. percent-events-correct-label (setText (format "%.2f%%" (:PEC results))))
      (. percent-events-wrong-label (setText (format "%.2f%%" (:PEW results))))
      (. accuracy-label (setText (format "%.2f" (:Acc results))))
      (. idcorrect-label (setText (format "%.2f" (:IDCorrect results))))
      (. accuracy-label (setText (format "%.2f" (:Acc results))))
      (. unexp-label (setText (format "%.2f%%" (:UnexplainedPct results)))))
    (do
      (. percent-events-correct-label (setText "N/A"))
      (. percent-events-wrong-label (setText "N/A"))
      (. accuracy-label (setText "N/A"))
      (. idcorrect-label (setText "N/A"))
      (. unexp-label (setText "N/A")))))

(defn player-get-truedata-log
  []
  (if (<= @time-now 1) ""
      (format-movements-comparative
       @truedata (:believed-movements (:problem-data (:ep-state @or-state)))
       (max 0 (dec @time-prev)) (dec @time-now))))

(defn move-str
  [mov]
  (format "%d,%d@%d -> %d,%d@%d (%s)" (:ox mov) (:oy mov) (:ot mov)
          (:x mov) (:y mov) (:time mov) (color-str (:color mov))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))
        believed-movements (sort-by :time (:believed-movements pdata))
        entities (:entities pdata)
        entity-biases (:entity-biases pdata)
        lines (fn [ss] (apply str (interpose "\n" ss)))]
    (format "Entity locations:\n%s\n\nBelieved movements:\n%s\n"
            (lines (map (fn [e] (let [loc (last (get entities e))]
                                  (format "%s (%s, %s): %d,%d@%d (from %s)"
                                          e (color-str (:color loc))
                                          (if-let [b (get entity-biases e)]
                                            (format "%s/%s"
                                                    (name (:bias b)) (:id (:bias-hyp b)))
                                            "?")
                                          (:x loc) (:y loc) (:time loc)
                                          (if (:loc-hyp loc) (:id (:loc-hyp loc))
                                              "initial problem data"))))
                        (sort-by str (AlphanumComparator.)
                                 (keys entities))))
            (lines (map move-str
                        (sort-by :time believed-movements))))))
