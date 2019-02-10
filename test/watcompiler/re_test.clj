(ns watcompiler.re-test
  (:require [clojure.test :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.re :refer :all])
  (:import [watcompiler.nfa NFA]))

;; Individual NFA tests
(deftest int-test
  (is (= :KEYWORD (run-NFA int-nfa "int")))
  (is (= :INTEGER (run-NFA integer-literal-nfa "109"))))

;; Merged NFA test
(deftest merged-test
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
  ;; Keywords
  ;; (is (= :KEYWORD (run-NFA complete-nfa "int"))))

(deftest boolean-test
  (is (= :BOOLEAN (run-NFA boolean-nfa "true")))
  (is (= :BOOLEAN (run-NFA boolean-nfa "false")))
  (is (= false (run-NFA boolean-nfa "tru")))
  (is (= false (run-NFA boolean-nfa "fals"))))

;; Keyword test
(deftest keyword-test
  ;; Individual Keywords on their nfas
  (is (= :KEYWORD (run-NFA int-nfa "int")))
  (is (= :KEYWORD (run-NFA abstract-nfa "abstract")))
  (is (= :KEYWORD (run-NFA default-nfa "default")))
  ;; All Test
  (is (= :KEYWORD (run-NFA keywords-nfa "abstract")))
  (is (= :KEYWORD (run-NFA keywords-nfa "default")))
  (is (= :KEYWORD (run-NFA keywords-nfa "if")))
  (is (= :KEYWORD (run-NFA keywords-nfa "private")))
  (is (= :KEYWORD (run-NFA keywords-nfa "this"))))
