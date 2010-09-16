(ns simulator.problems.circuit.player
  (:import (java.awt Color Graphics2D Image Dimension GridBagLayout Insets RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.imageio ImageIO))
  (:import (javax.swing JPanel JFrame JButton JTextField JTextArea
                        JLabel JScrollPane JSpinner SpinnerNumberModel JComboBox))
  (:import (java.io File))
  (:use [simulator.strategies :only (strategies)])
  (:use [simulator.problems.circuit.core
         :only (rand-gates-wiring make-input-vals save-graphviz)]))

(def *graphpng* nil)

(def *resultslabel* (JLabel. "Correct: "))

(def *param-spinners*
  {:MinGates (JSpinner. (SpinnerNumberModel. 3 3 1000 1))
   :MaxGates (JSpinner. (SpinnerNumberModel. 3 3 1000 1))
   :ProbBroken (JSpinner. (SpinnerNumberModel. 80 0 100 10))})

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
    (def *graphpng* resized)
    (println (/ (- 500.0 (.getWidth *graphpng*)) 2.0))))

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
  (let [[gates wiring] (rand-gates-wiring)
        input-vals (make-input-vals gates)]
    (save-graphviz "/home/josh/test.dot" "/home/josh/test.png" gates wiring)
    (update-graph)
    (. *graphpanel* (repaint))))

(def *newbutton*
     (let [b (JButton. "New")]
       (with-action b e (run-simulation))
       b))

(def *mainpanel*
  (doto (JPanel. (GridBagLayout.))
    (grid-bag-layout
     :fill :BOTH, :insets (Insets. 5 5 5 5)
     :gridx 0, :gridy 0, :gridheight 7
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

     :gridy 6, :gridwidth 2, :gridheight :REMAINDER
     (JPanel.))))

(defn start-player []
  (doto (JFrame. "Circuit player")
    (.setContentPane *mainpanel*)
    (.setResizable true)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.show)))
