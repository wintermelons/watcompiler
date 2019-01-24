(ns watcompiler.dfa-test
  (:require [clojure.test :refer :all]
            [watcompiler.dfa :refer :all]
            [watcompiler.lang :refer :all]))


(def run (partial run-DFA simple-dfa))

(deftest running-dfa
  (testing "identifiers"
    (is (true? (run "x")))
    (is (true? (run "x1")))
    (is (true? (run "xy")))
    (is (true? (run "my_3_good_var")))
    (is (true? (run "x12345")))
    (is (false? (run "123x")))
    (is (false? (run "3.x"))))
  (testing "numbers"
    (is (true? (run "123")))
    (is (true? (run "0")))
    (is (true? (run "3.14")))
    (is (true? (run "0.004")))
    (is (false? (run "00")))
    (is (false? (run "3.x")))
    (is (false? (run "03.14"))))
  (testing "operators"
    (is (true? (run "+")))
    (is (false? (run "-32")))) ;; single symbol only
  (testing "whitespace"
    (is (true? (run " "))))
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
  (is (= (list "0" 1 :INTEGER)
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
