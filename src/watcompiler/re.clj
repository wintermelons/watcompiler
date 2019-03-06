(ns watcompiler.re
  (:require [clojure.set :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all])
  (:import [watcompiler.nfa NFA]))

;; Helpers for merging nfas
(defn merge-nfas
  [& args]
  (apply merge args))

;; parse each string to form the nfa and then form complete nfa
;; form the states in the nfa
(defn string-to-nfa
  [word wordtype]
  (let
    [stateS (gensym :s)

      ;; List of substrings of word, stored as strings
      states-map (rest (reductions str (str) word))

      ;; Key: substring of word, Value: gensym associated with this state
      gensym-map (into (sorted-map) (for [c states-map]
                                         [c (gensym c)]))
      ;; Key: gensym value, Value: char to get to this state
      states-char-map (into (sorted-map)
                                        (for [pair (map list (vals gensym-map) (seq word))]
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

(def abstract-nfa (string-to-nfa "abstract" :KEYWORD))
(def default-nfa (string-to-nfa "default" :KEYWORD))
(def if-nfa (string-to-nfa "if" :KEYWORD))
(def private-nfa (string-to-nfa "private" :KEYWORD))
(def this-nfa (string-to-nfa "this" :KEYWORD))
(def boolean-nfa (string-to-nfa "boolean" :KEYWORD))
(def do-nfa (string-to-nfa "do" :KEYWORD))
(def implements-nfa (string-to-nfa "implements" :KEYWORD))
(def protected-nfa (string-to-nfa "protected" :KEYWORD))
(def break-nfa (string-to-nfa "break" :KEYWORD))
(def double-nfa (string-to-nfa "double" :KEYWORD))
(def import-nfa (string-to-nfa "import" :KEYWORD))
(def public-nfa (string-to-nfa "public" :KEYWORD))
(def throws-nfa (string-to-nfa "throws" :KEYWORD))
(def throw-nfa (string-to-nfa "throw" :KEYWORD))
(def byte-nfa (string-to-nfa "byte" :KEYWORD))
(def else-nfa (string-to-nfa "else" :KEYWORD))
(def instanceof-nfa (string-to-nfa "instanceof" :KEYWORD))
(def return-nfa (string-to-nfa "return" :KEYWORD))
(def transient-nfa (string-to-nfa "transient" :KEYWORD))
(def case-nfa (string-to-nfa "case" :KEYWORD))
(def extends-nfa (string-to-nfa "extends" :KEYWORD))
(def int-nfa (string-to-nfa "int" :KEYWORD))
(def short-nfa (string-to-nfa "short" :KEYWORD))
(def try-nfa (string-to-nfa "try" :KEYWORD))
(def catch-nfa (string-to-nfa "catch" :KEYWORD))
(def interface-nfa (string-to-nfa "interface" :KEYWORD))
(def static-nfa (string-to-nfa "static" :KEYWORD))
(def void-nfa (string-to-nfa "void" :KEYWORD))
(def char-nfa (string-to-nfa "char" :KEYWORD))
(def finally-nfa (string-to-nfa "finally" :KEYWORD))
(def final-nfa (string-to-nfa "final" :KEYWORD))
(def long-nfa (string-to-nfa "long" :KEYWORD))
(def strictfp-nfa (string-to-nfa "strictfp" :KEYWORD))
(def volatile-nfa (string-to-nfa "volatile" :KEYWORD))
(def class-nfa (string-to-nfa "class" :KEYWORD))
(def float-nfa (string-to-nfa "float" :KEYWORD))
(def native-nfa (string-to-nfa "native" :KEYWORD))
(def super-nfa (string-to-nfa "super" :KEYWORD))
(def while-nfa (string-to-nfa "while" :KEYWORD))
(def const-nfa (string-to-nfa "const" :KEYWORD))
(def for-nfa (string-to-nfa "for" :KEYWORD))
(def new-nfa (string-to-nfa "new" :KEYWORD))
(def switch-nfa (string-to-nfa "switch" :KEYWORD))
(def continue-nfa (string-to-nfa "continue" :KEYWORD))
(def goto-nfa (string-to-nfa "goto" :KEYWORD))
(def package-nfa (string-to-nfa "package" :KEYWORD))
(def synchronized-nfa (string-to-nfa "synchronized" :KEYWORD))

;; Keywords nfa
(def keywords-nfa
  (let [stateS (gensym :S)]
  ;; use default constructor because we no longer have the merged accept-map
  (->NFA (into #{} )
         (union
           abstract-nfa
           default-nfa
           if-nfa
           private-nfa
           this-nfa
           boolean-nfa
           do-nfa
           implements-nfa
           protected-nfa
           break-nfa
           double-nfa
           import-nfa
           public-nfa
           throws-nfa
           throw-nfa
           byte-nfa
           else-nfa
           instanceof-nfa
           return-nfa
           transient-nfa
           case-nfa
           extends-nfa
           int-nfa
           short-nfa
           try-nfa
           catch-nfa
           interface-nfa
           static-nfa
           void-nfa
           char-nfa
           finally-nfa
           final-nfa
           long-nfa
           strictfp-nfa
           volatile-nfa
           class-nfa
           float-nfa
           native-nfa
           super-nfa
           while-nfa
           const-nfa
           for-nfa
           new-nfa
           switch-nfa
           continue-nfa
           goto-nfa
           package-nfa
           synchronized-nfa)
         stateS
         (merge
           (:accept-states abstract-nfa)
           (:accept-states default-nfa)
           (:accept-states if-nfa)
           (:accept-states private-nfa)
           (:accept-states this-nfa)
           (:accept-states boolean-nfa)
           (:accept-states do-nfa)
           (:accept-states implements-nfa)
           (:accept-states protected-nfa)
           (:accept-states break-nfa)
           (:accept-states double-nfa)
           (:accept-states import-nfa)
           (:accept-states public-nfa)
           (:accept-states throws-nfa)
           (:accept-states throw-nfa)
           (:accept-states byte-nfa)
           (:accept-states else-nfa)
           (:accept-states instanceof-nfa)
           (:accept-states return-nfa)
           (:accept-states transient-nfa)
           (:accept-states case-nfa)
           (:accept-states extends-nfa)
           (:accept-states int-nfa)
           (:accept-states short-nfa)
           (:accept-states try-nfa)
           (:accept-states catch-nfa)
           (:accept-states interface-nfa)
           (:accept-states static-nfa)
           (:accept-states void-nfa)
           (:accept-states char-nfa)
           (:accept-states finally-nfa)
           (:accept-states final-nfa)
           (:accept-states long-nfa)
           (:accept-states strictfp-nfa)
           (:accept-states volatile-nfa)
           (:accept-states class-nfa)
           (:accept-states float-nfa)
           (:accept-states native-nfa)
           (:accept-states super-nfa)
           (:accept-states while-nfa)
           (:accept-states const-nfa)
           (:accept-states for-nfa)
           (:accept-states new-nfa)
           (:accept-states switch-nfa)
           (:accept-states continue-nfa)
           (:accept-states goto-nfa)
           (:accept-states package-nfa)
           (:accept-states synchronized-nfa))
         (merge
           (:transitions abstract-nfa)
           (:transitions default-nfa)
           (:transitions if-nfa)
           (:transitions private-nfa)
           (:transitions this-nfa)
           (:transitions boolean-nfa)
           (:transitions do-nfa)
           (:transitions implements-nfa)
           (:transitions protected-nfa)
           (:transitions break-nfa)
           (:transitions double-nfa)
           (:transitions import-nfa)
           (:transitions public-nfa)
           (:transitions throws-nfa)
           (:transitions throw-nfa)
           (:transitions byte-nfa)
           (:transitions else-nfa)
           (:transitions instanceof-nfa)
           (:transitions return-nfa)
           (:transitions transient-nfa)
           (:transitions case-nfa)
           (:transitions extends-nfa)
           (:transitions int-nfa)
           (:transitions short-nfa)
           (:transitions try-nfa)
           (:transitions catch-nfa)
           (:transitions interface-nfa)
           (:transitions static-nfa)
           (:transitions void-nfa)
           (:transitions char-nfa)
           (:transitions finally-nfa)
           (:transitions final-nfa)
           (:transitions long-nfa)
           (:transitions strictfp-nfa)
           (:transitions volatile-nfa)
           (:transitions class-nfa)
           (:transitions float-nfa)
           (:transitions native-nfa)
           (:transitions super-nfa)
           (:transitions while-nfa)
           (:transitions const-nfa)
           (:transitions for-nfa)
           (:transitions new-nfa)
           (:transitions switch-nfa)
           (:transitions continue-nfa)
           (:transitions goto-nfa)
           (:transitions package-nfa)
           (:transitions synchronized-nfa)
           (make-transition-NFA [[stateS (:start abstract-nfa) e]
                                 [stateS (:start default-nfa) e]
                                 [stateS (:start if-nfa) e]
                                 [stateS (:start private-nfa) e]
                                 [stateS (:start this-nfa) e]
                                 [stateS (:start boolean-nfa) e]
                                 [stateS (:start do-nfa) e]
                                 [stateS (:start implements-nfa) e]
                                 [stateS (:start protected-nfa) e]
                                 [stateS (:start break-nfa) e]
                                 [stateS (:start double-nfa) e]
                                 [stateS (:start import-nfa) e]
                                 [stateS (:start public-nfa) e]
                                 [stateS (:start throws-nfa) e]
                                 [stateS (:start throw-nfa) e]
                                 [stateS (:start byte-nfa) e]
                                 [stateS (:start else-nfa) e]
                                 [stateS (:start instanceof-nfa) e]
                                 [stateS (:start return-nfa) e]
                                 [stateS (:start transient-nfa) e]
                                 [stateS (:start case-nfa) e]
                                 [stateS (:start extends-nfa) e]
                                 [stateS (:start int-nfa) e]
                                 [stateS (:start short-nfa) e]
                                 [stateS (:start try-nfa) e]
                                 [stateS (:start catch-nfa) e]
                                 [stateS (:start interface-nfa) e]
                                 [stateS (:start static-nfa) e]
                                 [stateS (:start void-nfa) e]
                                 [stateS (:start char-nfa) e]
                                 [stateS (:start finally-nfa) e]
                                 [stateS (:start final-nfa) e]
                                 [stateS (:start long-nfa) e]
                                 [stateS (:start strictfp-nfa) e]
                                 [stateS (:start volatile-nfa) e]
                                 [stateS (:start class-nfa) e]
                                 [stateS (:start float-nfa) e]
                                 [stateS (:start native-nfa) e]
                                 [stateS (:start super-nfa) e]
                                 [stateS (:start while-nfa) e]
                                 [stateS (:start const-nfa) e]
                                 [stateS (:start for-nfa) e]
                                 [stateS (:start new-nfa) e]
                                 [stateS (:start switch-nfa) e]
                                 [stateS (:start continue-nfa) e]
                                 [stateS (:start goto-nfa) e]
                                 [stateS (:start package-nfa) e]
                                 [stateS (:start synchronized-nfa) e]]))
         (merge
           (:accept-priorities abstract-nfa)
           (:accept-priorities default-nfa)
           (:accept-priorities if-nfa)
           (:accept-priorities private-nfa)
           (:accept-priorities this-nfa)
           (:accept-priorities boolean-nfa)
           (:accept-priorities do-nfa)
           (:accept-priorities implements-nfa)
           (:accept-priorities protected-nfa)
           (:accept-priorities break-nfa)
           (:accept-priorities double-nfa)
           (:accept-priorities import-nfa)
           (:accept-priorities public-nfa)
           (:accept-priorities throws-nfa)
           (:accept-priorities throw-nfa)
           (:accept-priorities byte-nfa)
           (:accept-priorities else-nfa)
           (:accept-priorities instanceof-nfa)
           (:accept-priorities return-nfa)
           (:accept-priorities transient-nfa)
           (:accept-priorities case-nfa)
           (:accept-priorities extends-nfa)
           (:accept-priorities int-nfa)
           (:accept-priorities short-nfa)
           (:accept-priorities try-nfa)
           (:accept-priorities catch-nfa)
           (:accept-priorities interface-nfa)
           (:accept-priorities static-nfa)
           (:accept-priorities void-nfa)
           (:accept-priorities char-nfa)
           (:accept-priorities finally-nfa)
           (:accept-priorities final-nfa)
           (:accept-priorities long-nfa)
           (:accept-priorities strictfp-nfa)
           (:accept-priorities volatile-nfa)
           (:accept-priorities class-nfa)
           (:accept-priorities float-nfa)
           (:accept-priorities native-nfa)
           (:accept-priorities super-nfa)
           (:accept-priorities while-nfa)
           (:accept-priorities const-nfa)
           (:accept-priorities for-nfa)
           (:accept-priorities new-nfa)
           (:accept-priorities switch-nfa)
           (:accept-priorities continue-nfa)
           (:accept-priorities goto-nfa)
           (:accept-priorities package-nfa)
           (:accept-priorities synchronized-nfa)))))

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
  (let [stateS (gensym :S)]
  ;; use default constructor because we no longer have the merged accept-map
  (->NFA (into #{} )
         (union integer-literal-nfa operators-nfa boolean-nfa keywords-nfa)
         stateS
         (merge (:accept-states integer-literal-nfa)
                     (:accept-states operators-nfa)
                     (:accept-states boolean-nfa)
                     (:accept-states keywords-nfa))
         (merge (:transitions integer-literal-nfa)
                (:transitions operators-nfa)
                (:transitions boolean-nfa)
                (:transitions keywords-nfa)
                (make-transition-NFA [[stateS (:start integer-literal-nfa) e]
                                      [stateS (:start operators-nfa) e]
                                      [stateS (:start boolean-nfa) e]
                                      [stateS (:start keywords-nfa) e]]))
         (merge (:accept-priorities integer-literal-nfa)
                (:accept-priorities operators-nfa)
                (:accept-priorities boolean-nfa)
                (:accept-priorities keywords-nfa)))))
