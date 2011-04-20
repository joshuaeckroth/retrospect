(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.overtone/vijual "0.2.1"]
                 [cc.artifice/clj-swing "0.1.2-SNAPSHOT"]
                 [org.satta/loom "0.1.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.3.0"]
                     [marginalia "0.5.0"]
                     [midje "1.1"]]
  :main retrospect.core
  :keep-non-project-classes true
  :java-source-path "src/jvm"
  :jvm-opts ["-Dawt.useSystemAAFontSettings=on" "-Xmx800m"])
