(ns watcompiler.nfa-test
  (:require [clojure.test :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all])
  (:import [watcompiler.nfa NFA]))

(def testing-nfa
  (NFA. #{}
        #{:A :B :C :D :I :J :X :Z}
        :A
        {}
        {(list :A e) #{:C}
         (list :B e) #{:A :Z}
         (list :C e) #{:C :D :B :X}
         (list :D e) #{:I}
         (list :I e) #{:J}
         (list :J e) #{:D}}))

(deftest closure-tests
  (testing "e-closure"
    (is (= #{:A :B :C :D :I :J :X :Z}
           (e-closure testing-nfa #{:A})))
    (is (= #{:A :B :C :D :I :J :X :Z}
           (e-closure testing-nfa #{:C})))
    (is (= #{:D :I :J}
           (e-closure testing-nfa #{:D})))
    (is (= #{:D :I :J}
           (e-closure testing-nfa #{:I})))))

(deftest running-nfa
  (is (= :INT (run-NFA simple-nfa "int")))
  (is (= :IDENTIFIER (run-NFA simple-nfa "int3")))
  (is (= :IDENTIFIER (run-NFA simple-nfa "in")))
  (is (= :ZERO (run-NFA simple-nfa "0")))
  (is (= :INTEGER (run-NFA simple-nfa "11"))))
