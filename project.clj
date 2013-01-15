(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.overtone/vijual "0.2.1"]
                 [cc.artifice/clj-swing "0.1.4"]
                 [seesaw "1.4.2"]
                 [cc.artifice/loom "0.1.2"]
                 [batik/batik-swing "1.6-1"]
                 [batik/batik-gvt "1.6-1"]
                 [batik/batik-util "1.6-1"]
                 [batik/batik-dom "1.6-1"]
                 [org.apache.httpcomponents/httpcore "4.1.4"]
                 [org.apache.httpcomponents/httpclient "4.1.2"]
                 [org.apache.commons/commons-math3 "3.0"]
                 [log4j/log4j "1.2.16"]
                 [junit/junit "4.10"]
                 [com.intellij/annotations "5.1"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]
                 [com.fifesoft/rsyntaxtextarea "2.0.2"]
                 [com.github.insubstantial/substance "7.1"]
                 [norsys/netica "504"]]
  :dev-dependencies [[lein-marginalia "0.7.0"]]
  :plugins [[lein-localrepo "0.4.1"]]
  :main retrospect.core
  :keep-non-project-classes true
  :java-source-paths ["src/jvm"]
  :warn-on-reflection false ;; Emit warnings on all reflection calls.
  :jvm-opts ["-Dawt.useSystemAAFontSettings=on" "-Xmx5000m" "-Xss5m"
             "-Djava.library.path=/home/josh/git/research/retrospect/netica"])
