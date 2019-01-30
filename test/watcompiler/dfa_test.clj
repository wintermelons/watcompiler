(ns watcompiler.dfa-test
  (:require [clojure.test :refer :all]
            [watcompiler.dfa :refer :all]
            [watcompiler.lang :refer :all]))


(def run (partial run-DFA simple-dfa))

(deftest running-dfa
  (testing "identifiers"
    (is (= :IDENTIFIER (run "x")))
    (is (= :IDENTIFIER (run "x1")))
    (is (= :IDENTIFIER (run "xy")))
    (is (= :IDENTIFIER (run "my_3_good_var")))
    (is (= :IDENTIFIER (run "x12345")))
    (is (false? (run "123x")))
    (is (false? (run "3.x"))))
  (testing "numbers"
    (is (= :INTEGER (run "123")))
    (is (= :ZERO (run "0")))
    (is (= :FLOAT (run "3.14")))
    (is (= :FLOAT (run "0.004")))
    (is (false? (run "00")))
    (is (false? (run "3.x")))
    (is (false? (run "03.14"))))
  (testing "operators"
    (is (= :OPERATOR (run "+")))
    (is (false? (run "-32")))) ;; single symbol only
  (testing "whitespace"
    (is (= :WHITESPACE (run " "))))
  (testing "multiple symbols"
    (is (false? (run "x * y / 3.14 +6"))))
  (testing "unknown symbols"
    (is (false? (run ";")))))


(def munch (partial munch-DFA simple-dfa))

(deftest munching-dfa
  (is (= (list "xyz" 3 :IDENTIFIER)
         (munch "xyz")))
  (is (= (list "xyz" 3 :IDENTIFIER)
         (munch "xyz + 123")))
  (is (= (list "x" 1 :IDENTIFIER)
         (munch "x * y / 3.14 +6")))
  (is (= (list "0" 1 :ZERO)
         (munch "00")))
  (is (= (list "1" 1 :INTEGER)
         (munch "1x")))
  (is (= (list "x1234" 5 :IDENTIFIER)
         (munch "x1234.5")))
  (is (= (list "3" 1 :INTEGER)
         (munch "3.x"))))

(def scan (partial scan-DFA simple-dfa))

(deftest scanning-dfa
  (is (= [(list "xyz" :IDENTIFIER)]
         (scan "xyz")))
  (is (= [(list "x" :IDENTIFIER)
          (list " " :WHITESPACE)
          (list "*" :OPERATOR)
          (list " " :WHITESPACE)
          (list "y1" :IDENTIFIER)
          (list " " :WHITESPACE)
          (list "/" :OPERATOR)
          (list "   " :WHITESPACE)
          (list "3.14" :FLOAT)
          (list " " :WHITESPACE)
          (list "+" :OPERATOR)
          (list "6" :INTEGER)]
         (scan "x * y1 /   3.14 +6")))
  (is (= [(list "+" :OPERATOR)
          (list "+" :OPERATOR)]
         (scan "++")))
  (is (nil? (scan "3.x"))))
