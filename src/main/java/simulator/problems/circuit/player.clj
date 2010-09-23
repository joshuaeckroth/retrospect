(ns simulator.problems.circuit.player
  (:import (java.awt Color Graphics2D Image Dimension GridBagLayout Insets RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.imageio ImageIO))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
                        JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox
                        JTable))
  (:import (javax.swing.table AbstractTableModel))
  (:import (java.io File))
  (:use [simulator.strategies :only (strategies)])
  (:use [simulator.problems.circuit.circuit
         :only [rand-gates-wiring make-input-vals transpose
                find-undetectable-broken-gates]])
  (:use [simulator.problems.circuit.graphviz :only (save-graphviz)]))

(def *graphpng* nil)
(def *gates* nil)
(def *wiring* nil)
(def *input-vals* nil)

(def *undetectable-label* (JLabel. "Undetectable broken gates: "))

(def *resultslabel* (JLabel. "Correct: "))

(def *param-spinners*
  {:MinGates (JSpinner. (SpinnerNumberModel. 3 3 1000 1))
   :MaxGates (JSpinner. (SpinnerNumberModel. 20 3 1000 1))
   :ProbBroken (JSpinner. (SpinnerNumberModel. 20 0 100 10))})

(def *params* (apply hash-map (flatten (for [k (keys *param-spinners*)] [k 0]))))

(def *param-other*
  {:Strategy (JComboBox. (to-array strategies))})

(defn get-parameters []
  (apply hash-map (flatten (for [k (keys *param-spinners*)]
			     [k (->> (k *param-spinners*)
				     .getModel .getNumber .intValue)]))))

(defn get-strategy [] (nth strategies (.getSelectedIndex (:Strategy *param-other*))))

;; from http://stuartsierra.com/2010/01/08/agents-of-swing

(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

;;; from http://stuartsierra.com/2010/01/05/taming-the-gridbaglayout

(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))

;;; from http://stuartsierra.com/2010/01/05/taming-the-gridbaglayout

(defmacro grid-bag-layout [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))

(defn update-graph []
  (let [img (. ImageIO (read (File. "/home/josh/test.png")))
        width (.getWidth img)
        height (.getHeight img)
        scale (float (/ 500.0 (max width height)))
        resized (.getScaledInstance img (* scale width) (* scale height)
                                    (. Image SCALE_SMOOTH))]
    (def *graphpng* resized)))

(defn update-labels []
  (let [undetectable (find-undetectable-broken-gates *gates* *wiring*)]
    (if (empty? undetectable)
      (.setText *undetectable-label* "Undetectable broken gates:")
      (.setText *undetectable-label*
                (format "Undetectable broken gates: %s (n=%d or %.0f%%)"
                        (apply str (interpose ", " (map str undetectable)))
                        (count undetectable)
                        (* 100.0 (/ (count undetectable)
                                    (count *gates*))))))))

(def *graphpanel*
  (doto (proxy [JPanel] []
          (paint [g]
                 (. g (setColor (. Color white)))
                 (. g (fillRect 0 0 500 500))
                 (if *graphpng*
                   (. g (drawImage *graphpng*
                                   (int (/ (- 500.0 (.getWidth *graphpng*)) 2.0))
                                   (int (/ (- 500.0 (.getHeight *graphpng*)) 2.0))
                                   nil)))))
    (.setPreferredSize (new Dimension 500 500))))

(defn run-simulation []
  (let [params (get-parameters)
        [gates wiring] (rand-gates-wiring params)
        input-vals (make-input-vals gates)]
    (def *gates* gates)
    (def *wiring* wiring)
    (def *input-vals* input-vals)
    (save-graphviz "/home/josh/test.dot" "/home/josh/test.png" gates wiring)
    (update-graph)
    (update-labels)
    (. *graphpanel* (repaint))))

(def *newbutton*
     (let [b (JButton. "New")]
       (with-action b e (run-simulation))
       b))

(def *mainpanel*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     :gridx 0, :gridy 0, :gridheight 8
     *graphpanel*

     :gridx 1, :gridy 0, :gridheight 1
     (JLabel. "MinGates:")
     :gridx 2, :gridy 0
     (:MinGates *param-spinners*)

     :gridx 1, :gridy 1
     (JLabel. "MaxGates:")
     :gridx 2, :gridy 1
     (:MaxGates *param-spinners*)

     :gridx 1, :gridy 2
     (JLabel. "ProbBroken:")
     :gridx 2, :gridy 2
     (:ProbBroken *param-spinners*)

     :gridx 1, :gridy 3
     (JLabel. "Strategy:")
     :gridx 2, :gridy 3
     (:Strategy *param-other*)

     :gridx 1, :gridy 4
     *newbutton*

     :gridx 1, :gridy 5
     *resultslabel*

     :gridx 1, :gridy 6, :gridwidth 2
     *undetectable-label*

     :gridy 7, :gridwidth 2, :gridheight :REMAINDER
     (JPanel.))))

(defn start-player []
  (doto (JFrame. "Circuit player")
    (.setContentPane *mainpanel*)
    (.setResizable true)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show)))
