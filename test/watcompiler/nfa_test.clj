(ns watcompiler.nfa-test
  (:require [clojure.test :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all])
  (:import [watcompiler.nfa NFA]
           [watcompiler.dfa DFA]))

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

(def conversion-nfa
  (NFA. #{\A \B}
        #{:1 :2 :3 :4 :5 :6}
        :1
        {:6 (list :faz 0)
         :1 (list :bar 1)
         :2 (list :foo 2)}
        {(list :1 e) #{:2}
         (list :2 \A) #{:1}
         (list :2 \B) #{:3}
         (list :3 \A) #{:5 :6}
         (list :3 e) #{:4}
         (list :4 \A) #{:1}
         (list :4 e) #{:2}
         (list :4 \B) #{:4}
         (list :5 e) #{:2}
         (list :5 \B) #{:4}}))

(def conversion-dfa
  (DFA. #{\A \B}
        #{#{:1 :2}
          #{:2 :3 :4}
          #{:1 :2 :5 :6}}
        #{:1 :2}
        {#{:1 :2} :bar
         #{:2 :3 :4} :foo
         #{:1 :2 :5 :6} :faz}
        {(list #{:1 :2} \A) #{:1 :2}
         (list #{:1 :2} \B) #{:2 :3 :4}
         (list #{:2 :3 :4} \A) #{:1 :2 :5 :6}
         (list #{:2 :3 :4} \B) #{:2 :3 :4}
         (list #{:1 :2 :5 :6} \A) #{:1 :2}
         (list #{:1 :2 :5 :6} \B) #{:2 :3 :4}}))

(deftest nfa-to-dfa
  (let [converted (NFA-to-DFA conversion-nfa)]
    (is (= (:alphabet conversion-dfa)
           (:alphabet converted)))
    (is (= (:states conversion-dfa)
           (:states converted)))
    (is (= (:start conversion-dfa)
           (:start converted)))
    (is (= (:accept-states conversion-dfa)
           (:accept-states converted)))
    (is (= (:transitions conversion-dfa)
           (:transitions converted)))))
