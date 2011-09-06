(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.overtone/vijual "0.2.1"]
                 [cc.artifice/clj-swing "0.1.3"]
                 [cc.artifice/loom "0.1.1"]
                 [com.ashafa/clutch "0.2.4"]]
  :dev-dependencies [[swank-clojure "1.3.0"]
                     [midje "1.1"]
                     [clj-stacktrace "0.2.1"]
                     [lein-marginalia "0.6.0"]]
  :hooks [leiningen.hooks.clj-stacktrace-test]
  :repl-options [:caught clj-stacktrace.repl/pst+]
  :clj-stacktrace {:color true}
  :main retrospect.core
  :keep-non-project-classes true
  :java-source-path "src/jvm"
  :jvm-opts ["-Dawt.useSystemAAFontSettings=on" "-Xmx1800m" "-Xss2m"
             "-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"])
