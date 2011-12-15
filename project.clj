(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.overtone/vijual "0.2.1"]
                 [cc.artifice/clj-swing "0.1.4"]
                 [cc.artifice/loom "0.1.1"]
                 [com.ashafa/clutch "0.2.4"]
                 [batik/batik-swing "1.6-1"]
                 [batik/batik-util "1.6-1"]
                 [batik/batik-dom "1.6-1"]]
  :dev-dependencies [[marginalia "0.7.0-SNAPSHOT"]
                     [lein-marginalia "0.7.0-SNAPSHOT"]
                     [radagast "1.1.0"]]
  :main retrospect.core
  :keep-non-project-classes true
  :autodoc {:name "retrospect", :page-title "retrospect API" :trim-prefix "retrospect."
            :web-src-dir "https://github.com/joshuaeckroth/retrospect/blob/"}
  :java-source-path "src/jvm"
  :radagast/ns-whitelist #"retrospect"
  :jvm-opts ["-Dawt.useSystemAAFontSettings=on" "-Xmx3000m" "-Xss4m"])
