(ns retrospect.problems.tracking.player
  (:import (java.awt Graphics2D Dimension GridBagLayout GridBagConstraints
                     BorderLayout RenderingHints BasicStroke Font Insets))
  (:import (java.awt.image BufferedImage))
  (:import (javax.swing JPanel JSpinner SpinnerNumberModel))
  (:use [clojure.contrib.math :only [floor ceil]])
  (:use [clj-swing.label])
  (:use [clj-swing.frame])
  (:use [clj-swing.panel])
  (:use [retrospect.problems.tracking.sensors :only [sees]])
  (:use [retrospect.problems.tracking.grid :only [grid-entities find-entity]])
  (:use [retrospect.problems.tracking.truedata :only
         [get-entity-movements]])
  (:use [retrospect.problems.tracking.hypotheses :only
         [paths-str paths-to-movements]])
  (:use [retrospect.colors])
  (:use [retrospect.state]))

(def diagram-width (ref nil))
(def diagram-height (ref nil))
(def grid-width (ref nil))
(def grid-height (ref nil))
(def grid-cell-width (ref nil))
(def grid-cell-height (ref nil))

(def percent-events-correct-label (label ""))
(def mean-time-with-label-label (label ""))
(def mean-label-counts-label (label ""))
(def mouse-xy (label "Grid ?, ?"))

(def param-spinners
     {:NumberEntities (JSpinner. (SpinnerNumberModel. 1 1 100 1))
      :MaxWalk (JSpinner. (SpinnerNumberModel. 1 1 100 1))
      :ProbNewEntities (JSpinner. (SpinnerNumberModel. 0 0 100 10))
      :GridWidth (JSpinner. (SpinnerNumberModel. 30 1 100 1))
      :GridHeight (JSpinner. (SpinnerNumberModel. 30 1 100 1))
      :SensorCoverage (JSpinner. (SpinnerNumberModel. 100 0 100 10))
      :SensorSeesColor (JSpinner. (SpinnerNumberModel. 100 0 100 10))})

(defn player-get-params
  []
  (for [k (keys param-spinners)]
    [k (->> (k param-spinners) .getModel .getNumber .intValue)]))

(defn player-set-params
  []
  (doseq [k (keys param-spinners)]
    (. (k param-spinners) setValue (k @params))))

(defn player-get-params-panel
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 0.0
          :fill :BOTH :insets (Insets. 5 5 5 5)
          _ (label "NumberEntities:")
          :gridx 1
          _ (:NumberEntities param-spinners)
          :gridx 0 :gridy 1
          _ (label "MaxWalk:")
          :gridx 1
          _ (:MaxWalk param-spinners)
          :gridx 0 :gridy 2
          _ (label "ProbNewEntities:")
          :gridx 1
          _ (:ProbNewEntities param-spinners)
          :gridx 0 :gridy 3
          _ (label "GridWidth:")
          :gridx 1
          _ (:GridWidth param-spinners)
          :gridx 0 :gridy 4
          _ (label "GridHeight:")
          :gridx 1
          _ (:GridHeight param-spinners)
          :gridx 0 :gridy 5
          _ (label "SensorCoverage:")
          :gridx 1
          _ (:SensorCoverage param-spinners)
          :gridx 0 :gridy 6
          _ (label "SensorSeesColor:")
          :gridx 1
          _ (:SensorSeesColor param-spinners)
          :gridy 7 :weighty 1.0
          _ (panel)]))

(defn draw-move [#^Graphics2D g oldx oldy newx newy color width]
  (let [oldpx (+ (* oldx @grid-cell-width) (/ @grid-cell-width 2))
	oldpy (+ (* oldy @grid-cell-height) (/ @grid-cell-height 2))
	newpx (+ (* newx @grid-cell-width) (/ @grid-cell-width 2))
	newpy (+ (* newy @grid-cell-height) (/ @grid-cell-height 2))]
    (doto g
      (.setColor color)
      (.setStroke (BasicStroke. width))
      (.drawLine oldpx oldpy newpx newpy))))

(defn draw-movements [#^Graphics2D g entity]
  (let [entity-at (fn [t] (find-entity (nth @truedata t) entity))]
    (doseq [t (range @time-prev @time-now)]
      (when-let [old-e (if (> t 0) (entity-at (dec t)))]
        (let [new-e (entity-at t)
              {ox :x oy :y} (meta old-e)
              {x :x y :y} (meta new-e)
              degree (int (* 255 (/ (- @time-now t) (- @time-now @time-prev))))
              width (double (+ 2 (* 5 (/ degree 255))))]
          (draw-move g ox oy x y (var-color degree) width))))))

(defn fill-cell [#^Graphics2D g x y color]
  (doto g
    (.setColor color)
    (.fillRect (* x @grid-cell-width)
               (* y @grid-cell-height)
               @grid-cell-width @grid-cell-height)))

(defn fill-cell-entities [#^Graphics2D g x y es]
  (let [width (ceil (/ @grid-cell-width (count es)))]
    (doseq [i (range (count es))]
      (let [e (nth es i)
            left (+ (* i width) (* x @grid-cell-width))
            top (* y @grid-cell-height)]
        (doto g
          ;; draw block
          (.setColor (:color (meta e)))
          (.fillRect left top width @grid-cell-height)
          ;; draw id
          (.setColor white)
          (.setFont (.. g getFont (deriveFont Font/BOLD (float 12.0))))
          (.drawString (str e) (+ 3 left) (+ 15 top))
          ;; draw movement
          (draw-movements e))))))

(defn draw-grid [g]
  (dorun
   (for [x (range @grid-width) y (range @grid-height)]
     (fill-cell g x y white)))
  (dorun
   (for [x (range @grid-width) y (range @grid-height)]
     (doseq [sensor @sensors]
       (when (sees sensor x y)
         (if (:sees-color (meta sensor))
           (fill-cell g x y yellow-alpha)
           (fill-cell g x y gray-alpha))))))
  (when (and @truedata (> @time-now 0))
    (let [es (grid-entities (nth @truedata (dec @time-now)))]
      (dorun
       (for [x (range @grid-width) y (range @grid-height)]
         (let [es-here (sort (filter #(and (= x (:x (meta %))) (= y (:y (meta %)))) es))]
           (when (not-empty es-here)
             (fill-cell-entities g x y es-here))))))))

(defn render [g]
  (dosync
   (alter diagram-width (constantly (.. g (getClipBounds) width)))
   (alter diagram-height (constantly (.. g (getClipBounds) height)))
   (alter grid-width (constantly (:GridWidth @params)))
   (alter grid-height (constantly (:GridHeight @params)))
   (alter grid-cell-width (constantly (ceil (/ @diagram-width @grid-width))))
   (alter grid-cell-height (constantly (ceil (/ @diagram-height @grid-height)))))
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
              (format "Grid %d, %d" x y))))))))

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
          _ (label "PercentEventsCorrect:")
          :gridx 1
          _ percent-events-correct-label
          :gridx 0 :gridy 1
          _ (label "MeanTimeWithLabel:")
          :gridx 1
          _ mean-time-with-label-label
          :gridx 0 :gridy 2
          _ (label "MeanLabelCounts:")
          :gridx 1
          _ mean-label-counts-label
          :gridx 0 :gridy 3
          _ mouse-xy
          :gridy 4 :weighty 1.0
          _ (panel)]))

(defn player-update-stats
  []
  (if (> @time-now 0)
    (let [t (int (/ (dec @time-now) (:StepsBetween @params)))]
      (. percent-events-correct-label
         (setText
          (format "%.2f%%"
                  (:PEC (get (:results @or-state) t)))))
      (. mean-time-with-label-label
         (setText
          (format "%.2f"
                  (:MTL (get (:results @or-state) t)))))
      (. mean-label-counts-label
         (setText
          (format "%.2f"
                  (:MLC (get (:results @or-state) t))))))
    (do
      (. percent-events-correct-label (setText "N/A"))
      (. mean-time-with-label-label (setText "N/A"))
      (. mean-label-counts-label (setText "N/A")))))

(defn player-get-truedata-log
  []
  (if (<= @time-now 1) ""
      (get-entity-movements
       @truedata (max 0 (dec @time-prev)) (dec (dec @time-now))
       (paths-to-movements (:paths (:problem-data (:ep-state @or-state)))))))

(defn player-get-problem-log
  []
  (let [pdata (:problem-data (:ep-state @or-state))
        log (apply str (interpose "\n" (:log pdata)))]
    (format "%s\n\n%s" (paths-str (:paths pdata)) log)))
