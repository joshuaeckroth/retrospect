(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.overtone/vijual "0.2.1"]
                 [cc.artifice/clj-swing "0.1.4"]
                 [cc.artifice/loom "0.1.1"]
                 [com.ashafa/clutch "0.2.4"]
                 [batik/batik-swing "1.6-1"]
                 [batik/batik-gvt "1.6-1"]
                 [batik/batik-util "1.6-1"]
                 [batik/batik-dom "1.6-1"]
                 [org.apache.httpcomponents/httpcore "4.1.4"]
                 [org.apache.httpcomponents/httpclient "4.1.2"]
                 [log4j/log4j "1.2.16"]
                 [com.intellij/annotations "5.1"]]
  :main retrospect.core
  :keep-non-project-classes true
  :java-source-path "src/jvm"
  :radagast/ns-whitelist #"retrospect"
  :jvm-opts ["-Dawt.useSystemAAFontSettings=on" "-Xmx2000m" "-Xss2m"])
