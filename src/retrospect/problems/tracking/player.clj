(ns retrospect.problems.tracking.player
  (:import (misc AlphanumComparator))
  (:import (java.awt Graphics2D Dimension GridBagLayout GridBagConstraints
                     BorderLayout RenderingHints BasicStroke Font Insets Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JSpinner SpinnerNumberModel))
  (:use [clojure.contrib.math :only [floor ceil]])
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [retrospect.problems.tracking.sensors :only [sees]])
  (:use [retrospect.problems.tracking.movements :only
         [entities-at entity-movements]])
  (:use [retrospect.problems.tracking.truedata :only
         [format-movements-comparative]])
  (:use [retrospect.reason.abduction.problems.tracking.evaluate :only
         [get-true-movements]])
  (:use [retrospect.reason.abduction.workspace :only [lookup-hyp]])
  (:use [retrospect.problems.tracking.colors])
  (:use [retrospect.epistemicstates :only [cur-ep]])
  (:use [retrospect.state]))

(def resized (atom true)) ;; start at true so that, when loading, widths are calculated
(def diagram-true (ref nil))
(def diagram-believed (ref nil))

(def diagram-width (ref nil))
(def diagram-height (ref nil))
(def grid-width (ref nil))
(def grid-height (ref nil))
(def grid-cell-width (ref nil))
(def grid-cell-height (ref nil))

(def tpr-label (label ""))
(def fpr-label (label ""))
(def f1-label (label ""))
(def noexp-label (label ""))
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
  (doseq [{:keys [x y ox oy time color]}
          (filter #(and (<= @time-prev (:ot %)) (>= @time-now (:time %))) movs)]
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

(defn draw-grid
  [g movs]
  (dorun
   (for [x (range @grid-width) y (range @grid-height)]
     (doseq [sensor @sensors]
       (when (sees sensor x y)
         (if (:sees-color (meta sensor))
           (fill-cell g x y yellow-alpha)
           (fill-cell g x y gray-alpha))))))
  (when (and @truedata (> @time-now 0))    
    (draw-movements g movs)))

(defn render
  [g movs]
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
    (draw-grid bg movs)
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))

(defn make-diagram
  [movs-fn]
  (doto (proxy [JPanel] []
          (paint [g] (render g (movs-fn))))
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
  (when @diagram-true
    (.repaint @diagram-true))
  (when @diagram-believed
    (.repaint @diagram-believed)))

(defn player-setup-diagram
  []
  (dosync (alter diagram-true
                 (constantly (make-diagram #(get-true-movements @truedata @time-now))))
          (alter diagram-believed
                 (constantly (make-diagram
                              #(when (> @time-now 0)
                                 (let [ws (:workspace (cur-ep (:est @or-state)))]
                                   (map (fn [hypid] (:mov (lookup-hyp ws hypid)))
                                      (:movement (:accepted ws)))))))))
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ @diagram-true          
          :gridy 1 
          _ @diagram-believed]))

(defn player-get-stats-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "TPR:")
          :gridx 1
          _ tpr-label
          :gridx 0 :gridy 1
          _ (label "FPR:")
          :gridx 1
          _ fpr-label
          :gridx 0 :gridy 2
          _ (label "F1:")
          :gridx 1
          _ f1-label
          :gridx 0 :gridy 3
          _ (label "NoExplainers:")
          :gridx 1
          _ noexp-label
          :gridx 0 :gridy 4
          _ mouse-xy]))

(defn player-update-stats
  []
  (if-let [results (last (:results (cur-ep (:est @or-state))))]
    (do
      (. tpr-label (setText (format "%.2f" (:TPR results))))
      (. fpr-label (setText (format "%.2f" (:FPR results))))
      (. f1-label (setText (format "%.2f" (:F1 results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do
      (. tpr-label (setText "N/A"))
      (. fpr-label (setText "N/A"))
      (. f1-label (setText "N/A"))
      (. noexp-label (setText "N/A")))))

(defn player-get-truedata-log
  []
  ;; TODO: fix so it's not specific to abduction
  (if (<= @time-now 0) ""
      (let [ws (:workspace (cur-ep (:est @or-state)))]
        (format-movements-comparative
         (:test @truedata)
         (map #(:mov (lookup-hyp ws %))
            (get (:accepted ws) :movement))
         (max 0 @time-prev) @time-now))))

(defn move-str
  [mov]
  (format "%d,%d -> %d,%d at time %d (%s)" (:ox mov) (:oy mov)
     (:x mov) (:y mov) (:time mov) (color-str (:color mov))))

(defn player-get-problem-log
  []
  (let [ws (:workspace (cur-ep (:est @or-state)))
        mov-hyps (sort-by :id (AlphanumComparator.)
                          (map #(lookup-hyp ws %) (get (:accepted ws) :movement)))
        lines (fn [ss] (apply str (interpose "\n" ss)))]
    (format "Believed movements:\n%s" (lines (map #(format "%s: %s" % (move-str (:mov %)))
                                           mov-hyps)))))
