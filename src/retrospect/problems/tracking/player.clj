(ns retrospect.problems.tracking.player
  (:import (misc AlphanumComparator))
  (:import (java.awt Graphics2D Dimension GridBagLayout GridBagConstraints
                     BorderLayout RenderingHints BasicStroke Font Insets Color))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JSpinner SpinnerNumberModel))
  (:use [clj-swing.label])
  (:use [clj-swing.panel])
  (:use [retrospect.problems.tracking.sensors :only [sees]])
  (:use [retrospect.problems.tracking.movements :only
         [entities-at entity-movements]])
  (:use [retrospect.problems.tracking.truedata :only
         [format-movements-comparative]])
  (:use [retrospect.problems.tracking.hypotheses :only [get-kb]])
  (:use [retrospect.problems.tracking.evaluate :only
         [get-true-movements]])
  (:use [retrospect.reason.abduction.workspace :only [accepted]])
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

(def recall-label (label ""))
(def prec-label (label ""))
(def unexp-label (label ""))
(def noexp-label (label ""))
(def mouse-xy (label "Grid ?, ?"))

(defn draw-move
  [#^Graphics2D g oldx oldy newx newy color end-point?]
  (let [oldpx (+ (* oldx @grid-cell-width) (/ @grid-cell-width 2))
	oldpy (+ (* oldy @grid-cell-height) (/ @grid-cell-height 2))
	newpx (+ (* newx @grid-cell-width) (/ @grid-cell-width 2))
	newpy (+ (* newy @grid-cell-height) (/ @grid-cell-height 2))]
    (doto g
      (.setColor (Color. (.getRed color) (.getGreen color) (.getBlue color) 150))
      (.setStroke (BasicStroke. 2))
      (.drawLine oldpx oldpy newpx newpy))
    (when end-point?
      (.drawOval g newpx newpy 3 3))))

(defn draw-movements
  [#^Graphics2D g movs]
  (doseq [{:keys [x y ox oy time color]}
          (filter #(and (<= @time-prev (:ot %)) (>= @time-now (:time %))) movs)]
    (draw-move g ox oy x y color (= @time-now time))))

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
           (fill-cell g x y (Color. 252 233 79 100))
           (fill-cell g x y (Color. 211 215 207 100)))))))
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
     (alter grid-cell-width (constantly (int (Math/floor (/ @diagram-width @grid-width)))))
     (alter grid-cell-height (constantly (int (Math/floor (/ @diagram-height @grid-height))))))
    (swap! resized (constantly nil)))
  (let [img (new BufferedImage @diagram-width @diagram-height
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setRenderingHint (. RenderingHints KEY_ANTIALIASING)
                         (. RenderingHints VALUE_ANTIALIAS_ON))
      (.setColor (Color. 255 255 255))
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
         (when (and (not= 0 @grid-cell-width) (not= 0 @grid-cell-height))
           (. mouse-xy setText
              (let [x (int (Math/floor (/ (.getX e) @grid-cell-width)))
                    y (int (Math/floor (/ (.getY e) @grid-cell-height)))]
                (format "Grid %d, %d" x y)))))))
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
                                   (map :mov (:movement (accepted ws)))))))))
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
          _ (label "Prec:")
          :gridx 1
          _ prec-label
          :gridx 0 :gridy 1
          _ (label "Recall:")
          :gridx 1
          _ recall-label
          :gridx 0 :gridy 2
          _ (label "Unexplained:")
          :gridx 1
          _ unexp-label
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
      (. recall-label (setText (format "%.2f" (:Recall results))))
      (. prec-label (setText (format "%.2f" (:Prec results))))
      (. unexp-label (setText (format "%.2f" (:UnexplainedPct results))))
      (. noexp-label (setText (format "%.2f" (:NoExplainersPct results)))))
    (do
      (. recall-label (setText "N/A"))
      (. prec-label (setText "N/A"))
      (. unexp-label (setText "N/A"))
      (. noexp-label (setText "N/A")))))

(defn player-get-truedata-log
  []
  ;; TODO: fix so it's not specific to abduction
  (if (<= @time-now 0) ""
      (let [ws (:workspace (cur-ep (:est @or-state)))]
        (format-movements-comparative
         (:test @truedata)
         (map :mov (:movement (accepted ws)))
         (max 0 @time-prev) @time-now))))

(defn move-str
  [mov]
  (format "%d,%d -> %d,%d at time %d (%s)" (:ox mov) (:oy mov)
     (:x mov) (:y mov) (:time mov) (color-str (:color mov))))

(defn player-get-problem-log
  []
  (let [ws (:workspace (cur-ep (:est @or-state)))
        mov-hyps (sort-by :id (AlphanumComparator.) (:movement (accepted ws)))
        lines (fn [ss] (apply str (interpose "\n" ss)))
        kb (get-kb (accepted ws))]
    (format "Believed movements:\n%s"
       (lines (map #(format "%s: %s" % (move-str (:mov %))) mov-hyps)))))
