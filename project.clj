(defproject cc.artifice/retrospect "1.0.0-SNAPSHOT"
  :description "retrospect"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [incanter "1.2.3-SNAPSHOT"]
                 [org.clojars.overtone/vijual "0.2.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [marginalia "0.5.0"]
                     [clj-stacktrace "0.2.1"]]
  :main retrospect.core
  :source-path "src"
  :test-path "test"
  :library-path "lib"
  :resources-path "resources"
  :target-dir "target"
  :keep-non-project-classes true)

; :jvm-opts ["-agentlib:jdwp=transport=dt_socket,address=8021,server=y,suspend=n"]
