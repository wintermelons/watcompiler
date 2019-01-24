# [CS 444](https://www.student.cs.uwaterloo.ca/~cs444/): Compilers

A compiler for [Joos](https://www.student.cs.uwaterloo.ca/~cs444/joos.html), a subset of Java 1.3, to i386 assembly language (the Netwide Assembler dialect). Written in Clojure by David and Jon.

## Installation & Usage

Make sure you have Java (1.8+), and install [Leiningen](https://leiningen.org/).

    $ lein run program.java  # run compiler against a java file
    $ lein test              # run all tests
    $ lein uberjar           # compiler to a JAR

