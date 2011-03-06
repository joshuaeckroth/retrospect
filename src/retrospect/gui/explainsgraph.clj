(ns retrospect.gui.explainsgraph
  (:import (java.awt Color GridBagLayout Insets))
  (:import (javax.swing ImageIcon JViewport JLabel))
  (:use [clojure.java.shell :only [sh]])
  (:use [clj-swing.core :only [add-action-listener]])
  (:use [clj-swing.label])
  (:use [clj-swing.button])
  (:use [clj-swing.panel])
  (:use [retrospect.state]))

(def index (ref 0))
(def total (ref 0))

(defn draw-explains-graph
  []
  (if @or-state
    (let [i @index
          c @total
          dot (:dot (:workspace (:ep-state @or-state)))
          filename (str "explains-graphs/" i)]
      (when (not-empty dot)
        (spit (str filename ".dot") (nth dot i))
        #_(sh "dot" "-Tpng" (str "-o" filename ".png") (str filename ".dot"))
        #_(sh "mogrify" "-resize" "50%" (str filename ".png"))
        (str filename ".png")))))

(defn get-explains-graph-viewport
  []
  (if-let [eg (draw-explains-graph)]
    (let [imageicon (ImageIcon. eg)]
      (.. imageicon getImage flush)
      (doto (JViewport.)
        (.setBackground Color/white)
        (.setView (JLabel. imageicon))))))

(def scroll (scroll-panel (doto (JViewport.) (.setBackground Color/white))))
(def el (label ""))
(def pb (button "Prev" :enabled false))
(def nb (button "Next" :enabled false))

(defn set-label-text
  []
  (if (> @total 0)
    (. el setText (format "Cycle %d of %d" (inc @index) @total))
    (. el setText "")))

(defn prev-explains-graph
  []
  (dosync
    (when (= 0 @index) (. pb setEnabled false))
    (. nb setEnabled true)
    (when (> @index 0)
      (alter index dec)
      (set-label-text el)
      (. scroll setViewport (get-explains-graph-viewport)))))

(defn next-explains-graph
  []
  (dosync
   (when (= (dec @total) @index) (. nb setEnabled false))
   (. pb setEnabled true)
   (when (< @index (dec @total))
     (alter index inc)
     (set-label-text el)
     (. scroll setViewport (get-explains-graph-viewport)))))

(defn update-explains-graph
  []
  (dosync
   (alter index (constantly 0))
   (alter total (constantly 0)
          ;; TODO FIX
          #_(constantly (:total (:dot (:workspace (:ep-state @or-state))))))
   (. scroll setViewport (get-explains-graph-viewport))
   (. pb setEnabled false)
   (if (< 1 @total)
     (. nb setEnabled true)
     (. nb setEnabled false))))

(defn explains-graph-tab
  []
  (panel :layout (GridBagLayout.)
         :constrains (java.awt.GridBagConstraints.)
         [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 6 :insets (Insets. 5 5 5 5)
          _ scroll
          :gridx 0 :gridy 1 :weightx 0.0 :weighty 0.0 :gridwidth 1
          _ (doto pb (add-action-listener ([_] (prev-explains-graph))))
          :gridx 1
          _ (doto nb (add-action-listener ([_] (next-explains-graph))))
          :gridx 2 :weightx 1.0
          _ el
          :gridx 3 :weightx 0.0
          _ (label " unexplained " :foreground Color/orange)
          :gridx 4
          _ (label " accepted " :foreground Color/blue)
          :gridx 5
          _ (label " rejected " :foreground Color/red)]))

