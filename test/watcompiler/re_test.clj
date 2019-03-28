(ns watcompiler.re-test
  (:require [clojure.test :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.re :refer :all])
  (:import [watcompiler.nfa NFA]))

;; Regex NFA tests
(deftest integer-literal-tests
  (is (= :INTEGER-LITERAL (run-NFA integer-literal-nfa "1010")))
  (is (= :INTEGER-LITERAL (run-NFA integer-literal-nfa "0")))
  (is (= :INTEGER-LITERAL (run-NFA integer-literal-nfa "1"))))

(deftest string-literal-tests
  (is (= :STRING-LITERAL (run-NFA string-literal-nfa "\"s\"")))
  (is (= :STRING-LITERAL (run-NFA string-literal-nfa "\"thisis a string literal\"")))
  (is (= :STRING-LITERAL (run-NFA string-literal-nfa "\"[]~`!%^&*$(&^%#.][`  @$g literal\"")))
  (is (= :STRING-LITERAL (run-NFA string-literal-nfa "\"\"")))

  (is (= :STRING-LITERAL (run-NFA string-literal-nfa "\" \\b \\t \\n \\f \\r \\' \\\\ \"")))
  (is (= false (run-NFA string-literal-nfa "needquotes"))))

(deftest character-literal-tests
  (is (= :CHARACTER-LITERAL (run-NFA character-literal-nfa "'s'")))
  (is (= :CHARACTER-LITERAL (run-NFA character-literal-nfa "'\\b'")))
  (is (= :CHARACTER-LITERAL (run-NFA character-literal-nfa "'0'")))
  (is (= false (run-NFA string-literal-nfa "'sa'"))))

(deftest identifier-tests
  (is (= :IDENTIFIER (run-NFA identifier-nfa "thisidentifier")))
  (is (= :IDENTIFIER (run-NFA identifier-nfa "a"))))

(deftest whitespace-test
  (is (= :WHITESPACE (run-NFA whitespace-nfa "   \n\n"))))

;; Form the NFAs from a file
(deftest read-file
  (def lines readFile)
  (def formed fileFormed-nfa)

  (is (= :BRACKET (run-NFA formed "]")))
  (is (= :BOOLEAN-LITERAL (run-NFA formed "true")))
  (is (= :BRACKET (run-NFA formed "{"))))

;; Test forming multiple nfas from multiple strings
(deftest multiple-nfas-function-test
  (def full-nfa (form-multiple-nfas :KEYWORD "int" "if"))
  (is (= :KEYWORD (run-NFA full-nfa "int")))
  (is (= :KEYWORD (run-NFA full-nfa "if")))
  (is (= false (run-NFA full-nfa "in")))
  (is (= false (run-NFA full-nfa "nt"))))

;; Test function forming individual nfa
(deftest function-test
  (def int-nfa-test (string-to-nfa :INT "int"))
    (is :MAP int-nfa-test)
    (is (= :INT (run-NFA int-nfa-test "int")))
  (def synchronized-nfa-test (string-to-nfa :KEYWORD "synchronized"))
    (is :MAP synchronized-nfa-test)
    (is (= :KEYWORD (run-NFA synchronized-nfa-test "synchronized")))
    (is (= false (run-NFA synchronized-nfa-test "synchronize")))
    (is (= false (run-NFA synchronized-nfa-test "ynchronize"))))

;; Individual NFA tests
(deftest int-test
  (def int-nfa (string-to-nfa :KEYWORD "int"))
  (is (= :KEYWORD (run-NFA complete-nfa "int")))
  (is (= :INTEGER-LITERAL (run-NFA complete-nfa "109"))))

(deftest operator-test
  ;; Operators
  (is (= :BINARYOPERATOR (run-NFA complete-nfa "+")))
  (is (= :UNARYOPERATOR (run-NFA complete-nfa "++")))
  (is (= :BINARYOPERATOR (run-NFA complete-nfa ">")))
  (is (= :ASSIGNMENTOPERATOR (run-NFA complete-nfa ">>>=")))
  (is (= :UNARYOPERATOR (run-NFA complete-nfa "!")))
  (is (= :BINARYOPERATOR (run-NFA complete-nfa "!="))))

;; Booleans test
(deftest boolean-test
  (is (= :BOOLEAN-LITERAL (run-NFA complete-nfa "true")))
  (is (= :BOOLEAN-LITERAL (run-NFA complete-nfa "false")))
  (is (= :IDENTIFIER (run-NFA complete-nfa "tru")))
  (is (= :IDENTIFIER (run-NFA complete-nfa "fals"))))

;; Keyword test
(deftest keyword-test
  ;; Individual Keywords on their nfas
  (def int-nfa (string-to-nfa :KEYWORD "int"))
  (def abstract-nfa (string-to-nfa :KEYWORD "abstract"))
  (def default-nfa (string-to-nfa :KEYWORD "default"))
  (def synchronize-nfa (string-to-nfa :KEYWORD "synchronize"))
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
  (is (= :INTEGER-LITERAL (run-NFA complete-nfa "9")))
  (is (= :UNARYOPERATOR (run-NFA complete-nfa "++")))
  (is (= :BOOLEAN-LITERAL (run-NFA complete-nfa "true")))
  (is (= :BOOLEAN-LITERAL (run-NFA complete-nfa "false"))))

(deftest complete-nfa-test
  (is (= :KEYWORD (run-NFA complete-nfa "abstract")))
  (is (= :KEYWORD (run-NFA complete-nfa "default")))
  (is (= :KEYWORD (run-NFA complete-nfa "package")))
  (is (= :KEYWORD (run-NFA complete-nfa "synchronized")))
  ;; Booleans
  (is (= :BOOLEAN-LITERAL (run-NFA complete-nfa "true")))
  (is (= :BOOLEAN-LITERAL (run-NFA complete-nfa "false")))
  (is (= :IDENTIFIER (run-NFA complete-nfa "tru")))
  (is (= :IDENTIFIER (run-NFA complete-nfa "fals")))
  ;; Integer
  (is (= :INTEGER-LITERAL (run-NFA complete-nfa "109")))
  ;; Operators
  (is (= :BINARYOPERATOR (run-NFA complete-nfa "+")))
  (is (= :UNARYOPERATOR (run-NFA complete-nfa "++")))
  (is (= :BINARYOPERATOR (run-NFA complete-nfa ">")))
  (is (= :BINARYOPERATOR (run-NFA complete-nfa ">>>")))
  (is (= :ASSIGNMENTOPERATOR (run-NFA complete-nfa ">>>=")))
  ;; Terminal
  (is (= :TERMINAL (run-NFA complete-nfa ";")))
  ;; null
  (is (= :NULL-LITERAL (run-NFA complete-nfa "null"))))

(deftest filter-regex-nfas
  ;; INT-LITERAL in Tokens.txt
  ;; shouldn't give a real matching to the text given
  (is (= false (run-NFA complete-nfa "<numbers>"))))
