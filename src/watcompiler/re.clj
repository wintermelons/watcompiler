(ns watcompiler.re
  (:require [clojure.set :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all]
            [clojure.string :as str])
  (:import [watcompiler.nfa NFA]))

;; Merging multiple nfas
(defn merge-nfas
  [& nfas]
  (let
    [stateS (gensym :s)
     all-states (apply union (map :states nfas))
     all-accept-states (apply union (map :accept-states nfas))
     merged-transitions (apply merge (map :transitions nfas))
     all-transitions (merge
                       ;; Merged transitions from the nfas
                       merged-transitions
                       ;; Episilon transition to each nfa
                       (make-transition-NFA (into []
                                            (for [nfa-start (map :start nfas)]
                                                 [stateS nfa-start e]))))
     all-accept-priorities (apply union (map :accept-priorities nfas))]
    (->NFA (into #{} )
               all-states
               stateS
               all-accept-states
               all-transitions
               all-accept-priorities)))

;; Parses a string to form the nfa
(defn string-to-nfa
  [word wordtype]
  (let
    [stateS (gensym :s)

      ;; List of substrings of word, stored as strings
      states-map (set (rest (reductions str (str) word)))

      ;; Key: substring of word, Value: gensym associated with this state
      gensym-map (into (sorted-map) (for [c states-map]
                                         [c (gensym c)]))
      ;; Key: gensym value, Value: char to get to this state
      states-char-map (into (sorted-map) (for [pair (map list (vals gensym-map) (seq word))]
                                              [(first pair) (second pair)]))

      ;; Accept states
      accept-states-map {(get gensym-map word) (list wordtype 0)}

      ;; transitions from previous substring gensym to next substring gensym
      transitions-map (into #{ [stateS (get gensym-map (str (first (seq word))) \a) (first (seq word))] }
                      (for [v (partition 2 1 (vals gensym-map))]
                           [(first v) (second v) (get states-char-map (second v))]))]
    (make-NFA (into #{} )
              states-map
              stateS
              accept-states-map
              (make-transition-NFA transitions-map))))


;; Takes strings and forms nfas from them and links them into one nfa
(defn form-multiple-nfas
  [& arguments]
  (let
    [stateS (gensym :s)
     class (keyword (first arguments)) ;; Conver to a keyword since it reads strings from a file
     args (rest arguments)
     ;; Key: string for keyword, Value: NFA for that keyword
     strings-nfas (into (sorted-map) (for [nfa-name args]
                                          [nfa-name (string-to-nfa nfa-name class)]))
     all-states (apply union (map :states (vals strings-nfas)))
     all-accept-states (apply union (map :accept-states (vals strings-nfas)))
     merged-transitions (apply merge (map :transitions (vals strings-nfas)))
     all-transitions (merge
                       ;; Merged transitions from the nfas
                       merged-transitions
                       ;; Episilon transition to each nfa
                       (make-transition-NFA (into []
                                            (for [nfa-start (map :start (vals strings-nfas))]
                                                 [stateS nfa-start e]))))
     all-accept-priorities (apply union (map :accept-priorities (vals strings-nfas)))]
    (->NFA (into #{} )
               all-states
               stateS
               all-accept-states
               all-transitions
               all-accept-priorities)))

;; Reading the file
(def readFile
   (with-open [rdr (clojure.java.io/reader "src/watcompiler/Tokens.txt")]
     (reduce conj [] (line-seq rdr))))

;; Splitting the lines by space
(def splitLines
  (into []
  (for [x readFile]
       (str/split x #" "))))

(def fileFormed-nfa
  (let [nfas
    (into []
    (for [x splitLines]
         (apply form-multiple-nfas x)))]
    (apply merge-nfas nfas)))

;; NFAs for types

;; Integer literal
;; 0 and [1-9][0-9]*
(def integer-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)]
  (make-NFA (into #{} )
            #{stateS state1 state2}
            stateS
            {state2 (list :INTEGER 0)}
            (make-transition-NFA [[stateS state1 e]
                                  [state1 state2 DIGITS-NONZERO]
                                  [state2 state2 DIGITS]]))))

;; String literal
;; \".*\" (\ shown for escaping ")
(def string-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)]
  (make-NFA (into #{} )
            #{stateS state1 state2}
            stateS
            {state2 (list :STRING-LITERAL 0)}
            (make-transition-NFA [[stateS state1 "\""]
                                  [state1 state1 UPPER-ALPHABET]
                                  [state1 state1 LOWER-ALPHABET]
                                  [state1 state2 "\""]]))))

;; Identifiers
;; [a-zA-Z][a-zA-Z0-9]*
(def identifier-nfa
  (let [stateS        (gensym :S)
        state1        (gensym :s1)
        state2        (gensym :s2)]
  (make-NFA (into #{} )
            #{stateS state1 state2}
            stateS
            {state1  (list :IDENTIFIER 1)
             state2  (list :IDENTIFIER 1)}
            (make-transition-NFA [[stateS state1 UPPER-ALPHABET]
                                  [stateS state1 LOWER-ALPHABET]
                                  [state1 state2 UPPER-ALPHABET]
                                  [state1 state2 LOWER-ALPHABET]
                                  [state2 state2 UPPER-ALPHABET]
                                  [state2 state2 LOWER-ALPHABET]
                                  [state2 state2 DIGITS]]))))
;; Operators
(def operators-nfa
  (form-multiple-nfas :OPERATOR ">" "<" "<<" ">>" ">>>" "<<<" ">>>=" ">>="
                      ">=" "<=" "&" "&=" "=" "==" "!" "!=" "^=" "^" "+" "+="
                      "++" "-" "-=" "--" "*" "*=" "/" "/=" "%" "%="))

;; white space?

;; Keywords nfa
(def keywords-nfa
  (form-multiple-nfas :KEYWORD
                      "abstract"
                      "default"
                      "if"
                      "private"
                      "this"
                      "boolean"
                      "do"
                      "implements"
                      "protected"
                      "break"
                      "double"
                      "import"
                      "public"
                      "throws"
                      "throw"
                      "byte"
                      "else"
                      "instanceof"
                      "return"
                      "transient"
                      "case"
                      "extends"
                      "int"
                      "short"
                      "try"
                      "catch"
                      "interface"
                      "static"
                      "void"
                      "char"
                      "finally"
                      "final"
                      "long"
                      "strictfp"
                      "volatile"
                      "class"
                      "float"
                      "native"
                      "super"
                      "while"
                      "const"
                      "for"
                      "new"
                      "switch"
                      "continue"
                      "goto"
                      "package"
                      "synchronized"))

;; Booleans
(def boolean-nfa
  (form-multiple-nfas :BOOLEAN "true" "false"))

;; Brackets
(def bracket-nfa
  (form-multiple-nfas :BRACKET "{" "}" "(" ")" "[" "]"))

;; complete nfa from all of the individual RE nfas
;; int-literal
;; string-literal
;; identifiers
;; file specified nfas:
;; BRACKET
;; BOOLEAN
;; KEYWORD
;; UNARYOPERATOR
;; BINARYOPERATOR
;; ASSIGNMENTOPERATOR
;; TERMINAL
(def complete-nfa
  (merge-nfas integer-literal-nfa
              string-literal-nfa
              identifier-nfa
              fileFormed-nfa))
