(ns retrospect.gui.repl
  (:import (java.io PipedReader PipedWriter CharArrayWriter PrintWriter))
  (:import (java.awt GridBagLayout Insets Dimension Font))
  (:import (org.fife.ui.rtextarea RTextScrollPane))
  (:import (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants))
  (:use [clj-swing.panel])
  (:use [clj-swing.text-field])
  (:use [clj-swing.button])
  (:use [retrospect.state]))

(def repl-output (ref ""))

(def ^:dynamic *print-stack-trace-on-error* true)

(def editor (doto (RSyntaxTextArea.)
              (.setSyntaxEditingStyle SyntaxConstants/SYNTAX_STYLE_CLOJURE)))

;; adapted from http://code.google.com/p/enclojure-clojure-lib/
;;              source/browse/trunk/org.enclojure.repl/src/org/enclojure/repl/main.clj
(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
    (or
      (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
      (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn create-clojure-repl []
  "This function creates an instance of clojure repl using piped in and out.
   It returns a map of two functions repl-fn and result-fn - first function
   can be called with a valid clojure expression and the results are read using
   the result-fn."
  (let [cmd-wtr (PipedWriter.)
        result-rdr (PipedReader. 10000)
        piped-in (clojure.lang.LineNumberingPushbackReader. (PipedReader. cmd-wtr))
        piped-out (PrintWriter. (PipedWriter. result-rdr))
        repl-thread-fn #(binding [*in* piped-in
                                  *out* piped-out
                                  *err* (PrintWriter. *out*)]
                          (try
                            (clojure.main/repl
                             :init (fn [] (in-ns 'retrospect.state))
                             :read (fn [prompt exit] (read))
                             :print (fn [s] (println (format "--\n%s" (prn-str s))))
                             :caught (fn [e]
                                       (when (is-eof-ex? e)
                                         (throw e))
                                       (if *print-stack-trace-on-error*
                                         (.printStackTrace e *out*)
                                         (prn (clojure.main/repl-exception e)))
                                       (flush))
                             :need-prompt (constantly false))
                            (catch clojure.lang.LispReader$ReaderException ex
                              (prn "REPL closing"))
                            (catch java.lang.InterruptedException ex)
                            (catch java.nio.channels.ClosedByInterruptException ex)))]
    (.start (Thread. repl-thread-fn "REPL"))
    {:repl-fn (fn [cmd]
                (if (= cmd ":CLOSE-REPL")
                  (do
                    (.close cmd-wtr)
                    (.close result-rdr))
                  (do
                    (.write cmd-wtr cmd)
                    (.flush cmd-wtr))))
     ;;//??Using CharArrayWriter to build the string from each read of one byte
     ;;Once there is nothing to read than this function returns the string read.
     ;;Using partial so that CharArrayWriter is only created and once and reused.
     ;;There could be better way.
     :result-fn (partial
                 (fn [wtr]
                   (.write wtr (.read result-rdr))
                   (if (.ready result-rdr)
                     (recur wtr)
                     (let [result (.toString wtr)]
                       (.reset wtr)
                       result)))
                 (CharArrayWriter.))}))

(defn evaluate
  [repl-fn result-fn]
  (repl-fn (str "(do " (.getText editor) ")"))
  (dosync (alter repl-output str (result-fn))))

(defn update-repl-tab
  [])

(defn repl-tab
  []
  (let [{:keys [repl-fn result-fn]} (create-clojure-repl)]
    (dosync (alter repl-output (constantly (result-fn))))
    (doto (split-vertical           
           (scroll-panel (doto (text-area :str-ref repl-output :editable false :wrap true)
                           (.setFont (Font. "WenQuanYi Micro Hei" Font/PLAIN 12))))
           (panel :layout (GridBagLayout.)
                  :constrains (java.awt.GridBagConstraints.)
                  [:gridx 0 :gridy 0 :weightx 1.0 :weighty 1.0 :gridwidth 2
                   :fill :BOTH :insets (Insets. 5 0 5 0)
                   _ (RTextScrollPane. editor)
                   :gridx 0 :gridy 1 :weightx 1.0 :weighty 0.0 :gridwidth 1
                   _ (panel)
                   :gridx 1 :weightx 0.0
                   _ (button "Eval" :action ([_] (evaluate repl-fn result-fn)))])))))
