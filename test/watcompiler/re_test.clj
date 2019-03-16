(ns watcompiler.re-test
  (:require [clojure.test :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.re :refer :all])
  (:import [watcompiler.nfa NFA]))

;; Test forming multiple nfas from multiple strings
(deftest multiple-nfas-function-test
  (def full-nfa (form-multiple-nfas :KEYWORD "int" "if"))
  (is (= :KEYWORD (run-NFA full-nfa "int")))
  (is (= :KEYWORD (run-NFA full-nfa "if")))
  (is (= false (run-NFA full-nfa "in")))
  (is (= false (run-NFA full-nfa "nt"))))


;; Test function forming individual nfa
(deftest function-test
  (def int-nfa-test (string-to-nfa "int" :INT))
    (is :MAP int-nfa-test)
    (is (= :INT (run-NFA int-nfa-test "int")))
  (def synchronized-nfa-test (string-to-nfa "synchronized" :KEYWORD))
    (is :MAP synchronized-nfa-test)
    (is (= :KEYWORD (run-NFA synchronized-nfa-test "synchronized")))
    (is (= false (run-NFA synchronized-nfa-test "synchronize")))
    (is (= false (run-NFA synchronized-nfa-test "ynchronize"))))

;; Individual NFA tests
(deftest int-test
  (def int-nfa (string-to-nfa "int" :KEYWORD))
  (is (= :KEYWORD (run-NFA int-nfa "int")))
  (is (= :INTEGER (run-NFA integer-literal-nfa "109"))))

(deftest operator-test
  ;; Operators
  (is (= :OPERATOR (run-NFA operators-nfa "+")))
  (is (= :OPERATOR (run-NFA operators-nfa "++")))
  (is (= :OPERATOR (run-NFA operators-nfa ">")))
  (is (= :OPERATOR (run-NFA operators-nfa ">=")))
  (is (= :OPERATOR (run-NFA operators-nfa ">>")))
  (is (= :OPERATOR (run-NFA operators-nfa ">>=")))
  (is (= :OPERATOR (run-NFA operators-nfa ">>>")))
  (is (= :OPERATOR (run-NFA operators-nfa ">>>=")))
  (is (= :OPERATOR (run-NFA operators-nfa "&")))
  (is (= :OPERATOR (run-NFA operators-nfa "^=")))
  (is (= :OPERATOR (run-NFA operators-nfa "^")))
  (is (= :OPERATOR (run-NFA operators-nfa "<<")))
  (is (= :OPERATOR (run-NFA operators-nfa "=")))
  (is (= :OPERATOR (run-NFA operators-nfa "==")))
  (is (= :OPERATOR (run-NFA operators-nfa "!")))
  (is (= :OPERATOR (run-NFA operators-nfa "!="))))

;; Booleans test
(deftest boolean-test
  (is (= :BOOLEAN (run-NFA boolean-nfa "true")))
  (is (= :BOOLEAN (run-NFA boolean-nfa "false")))
  (is (= false (run-NFA boolean-nfa "tru")))
  (is (= false (run-NFA boolean-nfa "fals"))))

;; Keyword test
(deftest keyword-test
  ;; Individual Keywords on their nfas
  (def int-nfa (string-to-nfa "int" :KEYWORD))
  (def abstract-nfa (string-to-nfa "abstract" :KEYWORD))
  (def default-nfa (string-to-nfa "default" :KEYWORD))
  (def synchronize-nfa (string-to-nfa "synchronize" :KEYWORD))
  (is (= :KEYWORD (run-NFA int-nfa "int")))
  (is (= :KEYWORD (run-NFA abstract-nfa "abstract")))
  (is (= :KEYWORD (run-NFA default-nfa "default")))
  (is (= false (run-NFA synchronized-nfa-test "synchronize")))
  (is (= false (run-NFA synchronized-nfa-test "ynchronize"))))

;; Test on a complete merged nfa
(deftest merged-function-nfa-test
  (is :MAP complete-nfa)
  (is (= :KEYWORD (run-NFA complete-nfa "int")))
  (is (= :KEYWORD (run-NFA complete-nfa "synchronized")))
  (is (= :INTEGER (run-NFA complete-nfa "109")))
  (is (= :OPERATOR (run-NFA complete-nfa "++")))
  (is (= :BOOLEAN (run-NFA complete-nfa "true")))
  (is (= :BOOLEAN (run-NFA complete-nfa "false"))))

(deftest keywords-test
  (is (= :KEYWORD (run-NFA complete-nfa "abstract")))
  (is (= :KEYWORD (run-NFA complete-nfa "default")))
  (is (= :KEYWORD (run-NFA complete-nfa "if")))
  (is (= :KEYWORD (run-NFA complete-nfa "private")))
  (is (= :KEYWORD (run-NFA complete-nfa "this")))
  (is (= :KEYWORD (run-NFA complete-nfa "boolean")))
  (is (= :KEYWORD (run-NFA complete-nfa "do")))
  (is (= :KEYWORD (run-NFA complete-nfa "implements")))
  (is (= :KEYWORD (run-NFA complete-nfa "protected")))
  (is (= :KEYWORD (run-NFA complete-nfa "break")))
  (is (= :KEYWORD (run-NFA complete-nfa "double")))
  (is (= :KEYWORD (run-NFA complete-nfa "import")))
  (is (= :KEYWORD (run-NFA complete-nfa "public")))
  (is (= :KEYWORD (run-NFA complete-nfa "throws")))
  (is (= :KEYWORD (run-NFA complete-nfa "byte")))
  (is (= :KEYWORD (run-NFA complete-nfa "else")))
  (is (= :KEYWORD (run-NFA complete-nfa "instanceof")))
  (is (= :KEYWORD (run-NFA complete-nfa "return")))
  (is (= :KEYWORD (run-NFA complete-nfa "transient")))
  (is (= :KEYWORD (run-NFA complete-nfa "case")))
  (is (= :KEYWORD (run-NFA complete-nfa "extends")))
  (is (= :KEYWORD (run-NFA complete-nfa "int")))
  (is (= :KEYWORD (run-NFA complete-nfa "short")))
  (is (= :KEYWORD (run-NFA complete-nfa "try")))
  (is (= :KEYWORD (run-NFA complete-nfa "catch")))
  (is (= :KEYWORD (run-NFA complete-nfa "interface")))
  (is (= :KEYWORD (run-NFA complete-nfa "static")))
  (is (= :KEYWORD (run-NFA complete-nfa "void")))
  (is (= :KEYWORD (run-NFA complete-nfa "char")))
  (is (= :KEYWORD (run-NFA complete-nfa "finally")))
  (is (= :KEYWORD (run-NFA complete-nfa "long")))
  (is (= :KEYWORD (run-NFA complete-nfa "strictfp")))
  (is (= :KEYWORD (run-NFA complete-nfa "volatile")))
  (is (= :KEYWORD (run-NFA complete-nfa "class")))
  (is (= :KEYWORD (run-NFA complete-nfa "float")))
  (is (= :KEYWORD (run-NFA complete-nfa "native")))
  (is (= :KEYWORD (run-NFA complete-nfa "super")))
  (is (= :KEYWORD (run-NFA complete-nfa "while")))
  (is (= :KEYWORD (run-NFA complete-nfa "const")))
  (is (= :KEYWORD (run-NFA complete-nfa "for")))
  (is (= :KEYWORD (run-NFA complete-nfa "new")))
  (is (= :KEYWORD (run-NFA complete-nfa "switch")))
  (is (= :KEYWORD (run-NFA complete-nfa "continue")))
  (is (= :KEYWORD (run-NFA complete-nfa "goto")))
  (is (= :KEYWORD (run-NFA complete-nfa "package")))
  (is (= :KEYWORD (run-NFA complete-nfa "synchronized")))
  ;; Booleans
  (is (= :BOOLEAN (run-NFA complete-nfa "true")))
  (is (= :BOOLEAN (run-NFA complete-nfa "false")))
  (is (= :IDENTIFIER (run-NFA complete-nfa "tru")))
  (is (= :IDENTIFIER (run-NFA complete-nfa "fals")))
  ;; Integer
  (is (= :INTEGER (run-NFA complete-nfa "109")))
  ;; Operators
  (is (= :OPERATOR (run-NFA complete-nfa "+")))
  (is (= :OPERATOR (run-NFA complete-nfa "++")))
  (is (= :OPERATOR (run-NFA complete-nfa ">")))
  (is (= :OPERATOR (run-NFA complete-nfa ">=")))
  (is (= :OPERATOR (run-NFA complete-nfa ">>")))
  (is (= :OPERATOR (run-NFA complete-nfa ">>=")))
  (is (= :OPERATOR (run-NFA complete-nfa ">>>")))
  (is (= :OPERATOR (run-NFA complete-nfa ">>>=")))
  (is (= :OPERATOR (run-NFA complete-nfa "&")))
  (is (= :OPERATOR (run-NFA complete-nfa "^=")))
  (is (= :OPERATOR (run-NFA complete-nfa "^")))
  (is (= :OPERATOR (run-NFA complete-nfa "<<")))
  (is (= :OPERATOR (run-NFA complete-nfa "=")))
  (is (= :OPERATOR (run-NFA complete-nfa "==")))
  (is (= :OPERATOR (run-NFA complete-nfa "!")))
  (is (= :OPERATOR (run-NFA complete-nfa "!="))))
