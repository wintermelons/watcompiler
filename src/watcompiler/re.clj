(ns watcompiler.re
  (:require [clojure.set :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all])
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
  [& args]
  (let
    [stateS (gensym :s)
     ;; Key: string for keyword, Value: NFA for that keyword
     strings-nfas (into (sorted-map) (for [nfa-name args]
                                          [nfa-name (string-to-nfa nfa-name :KEYWORD)]))
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

;; NFAs for types

;; Integer literal
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
;; Operators
(def operators-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1) ;; valid, but can add =
        state2 (gensym :2) ;; valid, but can add <, =
        state3 (gensym :3) ;; valid but can add >, =
        state4 (gensym :4) ;; valid, but can add +
        state5 (gensym :5) ;; valid, but can add -
        state6 (gensym :6) ;; valid, but can add >, =
        state7 (gensym :7) ;; valid, but can add >, =
        state11 (gensym :11)] ;; Nothing else can be added to it
  (make-NFA (into #{} )
            #{stateS state1 state2, state3, state4, state5, state6, state11}
            stateS
            {state11 (list :OPERATOR 0)
             state1 (list :OPERATOR 1)
             state2 (list :OPERATOR 2)
             state3 (list :OPERATOR 3)
             state4 (list :OPERATOR 4)
             state5 (list :OPERATOR 5)
             state6 (list :OPERATOR 6)
             state7 (list :OPERATOR 7)}
            (make-transition-NFA [[stateS state1 \=]
                                  [stateS state2 \<]
                                  [stateS state3 \>]
                                  [stateS state1 \!]
                                  [stateS state1 \:]
                                  [stateS state1 \~]
                                  [stateS state1 \?]
                                  [stateS state1 \&]
                                  [stateS state1 \|]
                                  [stateS state1 \^]
                                  [stateS state1 \%]
                                  [stateS state4 \+]
                                  [stateS state5 \-]
                                  [stateS state1 \*]
                                  [stateS state1 \/]

                                  [state1 state11 \=]
                                  [state2 state1 \<]
                                  [state3 state6 \>]
                                  [state3 state11 \=]
                                  [state4 state11 \+]
                                  [state5 state11 \-]
                                  [state6 state7 \>]
                                  [state6 state11 \=]
                                  [state7 state11 \=]]))))

;; Keywords nfa
(def keywords-nfa
  (form-multiple-nfas "abstract"
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
  (let [stateS        (gensym :S)
        statet        (gensym :t)
        statetr       (gensym :tr)
        statetru      (gensym :tru)
        statetrue     (gensym :true)
        statef        (gensym :f)
        statefa       (gensym :fa)
        statefal      (gensym :fal)
        statefals     (gensym :fals)
        statefalse    (gensym :false)]
  (make-NFA (into #{} )
            #{stateS statet statetr statetru statetrue statef statefa statefal statefals statefalse}
            stateS
            {statetrue  (list :BOOLEAN 0)
             statefalse (list :BOOLEAN 0)}
            (make-transition-NFA [[stateS statet \t]
                                  [statet statetr \r]
                                  [statetr statetru \u]
                                  [statetru statetrue \e]
                                  [stateS statef \f]
                                  [statef statefa \a]
                                  [statefa statefal \l]
                                  [statefal statefals \s]
                                  [statefals statefalse \e]]))))

;; complete nfa from all of the individual RE nfas
;; int-literal
;; operators
;; boolean
;; keywords
(def complete-nfa
  (merge-nfas integer-literal-nfa operators-nfa boolean-nfa keywords-nfa))
