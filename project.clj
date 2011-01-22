(defproject cc.artifice/samre "1.0.0-SNAPSHOT"
  :description "SAMRE: Software for Abductive Meta-Reasoning Experimentation"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [incanter "1.2.3-SNAPSHOT"]
		 [org.clojars.choas/clojure-hadoop "1.2.1-SNAPSHOT"]
                 [org.clojars.overtone/vijual "0.2.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]
                     [marginalia "0.2.2"]]
  :autodoc {:name "SAMRE" :page-title "SAMRE API Documentation"
            :web-home "http://joshuaeckroth.github.com/SAMRE/"
            :copyright "Copyright 2010-2011 Joshua Eckroth. All rights reserved."
            :trim-prefix "samre."}
  :main samre.core
  :source-path "src"
  :test-path "test"
  :library-path "lib"
  :resources-path "resources"
  :target-dir "target"
  :keep-non-project-classes true)

; :jvm-opts ["-agentlib:jdwp=transport=dt_socket,address=8021,server=y,suspend=n"]
