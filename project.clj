(defproject clushjr "0.1-SNAPSHOT"
  :description "clushjr is for numerics, yo"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [incanter/parallelcolt "0.9.4"]
 ;;                [org.clojars.slocklin/jblas-static-sandybridge "1.2.1"]]
                   [kephale/jblas "1.2.0"]]
  :dev-dependencies [[lein-clojars "0.7.0"] [swank-clojure "1.3.4-SNAPSHOT"]]
  :java-source-path ["src"]
  :jvm-opts ["-Xmx1g"])

