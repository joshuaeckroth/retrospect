(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/data.xml "0.0.7" :exclusions [[org.clojure/clojure]]]
                 [cc.artifice/vijual "0.2.5"]
                 [cc.artifice/clj-swing "0.1.6"]
                 [cc.artifice/geppetto "2.5.0-SNAPSHOT"]
                 [prismatic/plumbing "0.1.0"]
                 [seesaw "1.4.4"]
                 [cc.artifice/loom "0.1.3"]
                 [org.clojure/tools.cli "0.2.4"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 [batik/batik-swing "1.6-1"]
                 [batik/batik-gvt "1.6-1"]
                 [batik/batik-util "1.6-1"]
                 [batik/batik-dom "1.6-1"]
                 [org.apache.httpcomponents/httpcore "4.3"]
                 [org.apache.httpcomponents/httpclient "4.3"]
                 [log4j/log4j "1.2.17"]
                 [junit/junit "4.11"]
                 [com.intellij/annotations "12.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [com.fifesoft/rsyntaxtextarea "2.0.7"]
                 [norsys/netica "504"]
                 [propertea "1.3.1"]
                 [fleet "0.9.5"]]
  :main retrospect.core
  :java-source-paths ["src/jvm"]
  :jvm-opts ["-Xmx2000m" "-Dawt.useSystemAAFontSettings=on" "-Djava.library.path=netica"])

