(ns watcompiler.lang
  (:require [watcompiler.dfa :refer :all])
  (:import [watcompiler.dfa DFA]))

(defn char-range 
  "Returns a vector of chars between start and end inclusive"
  [start end]
  (vec (map char (range (int start) (inc (int end))))))

(def LOWER-ALPHABET
  (char-range \a \z))

(def UPPER-ALPHABET
  (char-range \A \Z))

(def S-ZERO \0)

(def DIGITS
  (char-range \0 \9))

(def DIGITS-NONZERO
  (char-range \1 \9))

(def S-PLUS \+)
(def S-MINUS \-)
(def S-STAR \*)
(def S-SLASH \/)

(def OPERATORS
  [S-PLUS S-MINUS S-STAR S-SLASH])

(def S-UNDERSCORE \_)
(def S-DOT \.)

(def S-SPACE \space)
(def S-TAB \tab)
(def S-NEWLINE \newline)

(def WHITESPACE
  [S-SPACE S-TAB S-NEWLINE])


;; --- Simplified Language --- ;;
;; Used for testing purposes.
;; Includes integers, floats, alpha-numeric 
;; identifiers, math operators and whitespace.

(def simple-trans
  [[:S :1 OPERATORS]
   [:S :2 LOWER-ALPHABET]
   [:2 :2 (vec (concat LOWER-ALPHABET DIGITS [S-UNDERSCORE]))]
   [:S :3 DIGITS-NONZERO]
   [:3 :3 DIGITS]
   [:3 :4 S-DOT]
   [:4 :5 DIGITS]
   [:5 :5 DIGITS]
   [:S :6 WHITESPACE]
   [:6 :6 WHITESPACE]
   [:S :7 S-ZERO]
   [:7 :8 S-DOT]
   [:8 :9 DIGITS]
   [:9 :9 DIGITS]])

(def simple-dfa
  (DFA. (into #{} (concat LOWER-ALPHABET 
                          DIGITS 
                          OPERATORS 
                          WHITESPACE
                          [S-UNDERSCORE S-DOT]))
        #{:S :1 :2 :3 :4 :5 :6 :7 :8 :9}
        :S
        {:1 :OPERATOR
         :2 :IDENTIFIER
         :3 :INTEGER
         :5 :FLOAT
         :6 :WHITESPACE
         :7 :INTEGER
         :9 :FLOAT}
        (make-transition simple-trans)))

