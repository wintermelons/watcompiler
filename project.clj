(defproject watcompiler "0.1.0-SNAPSHOT"
  :description "CS 444 Project: Compiler for Joos 1.W, a subset of Java 1.3 language, to x86."
  :url "https://www.student.cs.uwaterloo.ca/~cs444/"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :main ^:skip-aot watcompiler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
