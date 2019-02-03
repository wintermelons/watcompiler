(ns watcompiler.cfg-test
  (:require [clojure.test :refer :all]
            [watcompiler.cfg :refer :all]
            [watcompiler.lang :refer :all]))


(def testing-cfg
  (make-CFG #{\( \)}
            #{:S}
            :S
            [(list :S (list :S :S))
             (list :S (list \( :S \)))
             (list :S (list))]))


(deftest derivations
  (testing "leftmost"
    (is (= (list (vec (char-array "(()(()))")) 7)
           (leftmost testing-cfg [1 0 1 2 1 1 2])))
    (is (= (list [\( :S \)] 1)
           (leftmost testing-cfg [1])))
    (is (= (list [\( \)] 2)
           (leftmost testing-cfg [1 2])))
    (is (= (list [\( \)] 2)
           (leftmost testing-cfg [1 2 0]))))
  (testing "rightmost"
    (is (= (list (vec (char-array "(()(()))")) 7)
           (rightmost testing-cfg [1 0 1 1 2 1 2])))
    (is (= (list [\( :S \)] 1)
           (rightmost testing-cfg [1])))
    (is (= (list [\( \)] 2)
           (rightmost testing-cfg [1 2])))
    (is (= (list [\( \)] 2)
           (rightmost testing-cfg [1 2 0])))
    (is (= (list (vec (char-array "((())()())()")) 13)
           (rightmost testing-cfg [0 1 2 1 0 0 1 2 1 2 1 1 2])))
    (is (= (list (vec (char-array "((())()())()")) 13)
           (rightmost testing-cfg [0 1 2 1 0 1 2 0 1 2 1 1 2])))))
