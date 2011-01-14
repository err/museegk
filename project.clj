(defproject museegk "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [clj-glob "0.1.0"]
		 [org.clojars.technomancy/rosado.processing "1.1.0"]
		 [overtone/scsynth-jna "0.1.2-SNAPSHOT"]
		 [overtone/osc-clj "0.3.0-SNAPSHOT"]
                 [overtone/byte-spec "0.2.0-SNAPSHOT"]
                 [overtone/midi-clj "0.2.0-SNAPSHOT"]
                 [org.clojars.overtone/vijual "0.2.1"]
                 [overtone/midi-clj "0.2.0-SNAPSHOT"]
		 [org.clojars.automata/rosado.processing "1.1.0"]
		 [java-tuio "cvs-SNAPSHOT"]]
  
;;  :dev-dependencies [[swank-clojure "1.2.1"]]

  :jvm-opts ["-Xms256m" "-Xmx1g" "-XX:+UseConcMarkSweepGC"])



