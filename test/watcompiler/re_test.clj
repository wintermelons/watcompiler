(ns watcompiler.re-test
  (:require [clojure.test :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.re :refer :all])
  (:import [watcompiler.nfa NFA]))

;; Individual NFA tests
(deftest int-test
  (is (= :INT (run-NFA int-nfa "int")))
  (is (= :INTEGER (run-NFA integer-literal-nfa "109"))))

;; Merged NFA test
(deftest merged-test
  (is (= :INT (run-NFA complete-nfa "int")))
  (is (= :INTEGER (run-NFA complete-nfa "109"))))
