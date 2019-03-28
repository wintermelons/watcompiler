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
  [wordtype word]
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
    (make-NFA (into #{} (concat (seq word)))
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
                                          [nfa-name (string-to-nfa class nfa-name)]))
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
  (into []
    (for [line (str/split-lines (slurp "src/watcompiler/Tokens.txt"))]
      (str/split line #" "))))

(def fileFormed-nfa
  (let [nfas (into []
               (for [x readFile]
                 ;; Check for regex based NFAs
                 (if
                   (or
                     (if (= (first x) "IDENTIFIER") true false)
                     (if (= (first x) "INT-LITERAL") true false)
                     (if (= (first x) "STRING-LITERAL") true false)
                     (if (= (first x) "CHARACTER-LITERAL") true false)
                     (if (= (first x) "WHITESPACE") true false))
                 nil
                 (apply form-multiple-nfas x))))]
  (apply merge-nfas (remove nil? nfas))))

;; NFAs for types

;; Integer literal
;; 0 and [1-9][0-9]*
(def integer-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)]
  (make-NFA (into #{} (concat [\0] DIGITS DIGITS-NONZERO))
            #{stateS state1 state2}
            stateS
            {state1 (list :INTEGER-LITERAL 0)
             state2 (list :INTEGER-LITERAL 0)}
            (make-transition-NFA [[stateS state1 \0]
                                  [stateS state2 DIGITS-NONZERO]
                                  [state2 state2 DIGITS]]))))

;; String literal
;; \"(\\[btnfr\"\'\\] | ALL-ASCII)*\" (\ shown for escaping ")
;; aka \"(.*)\" with escapes inside
(def string-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)
        state3 (gensym :3)]
  (make-NFA (into #{} (concat [\'] ALL-ASCII [\\] ESCAPABLE))
            #{stateS state1 state2 state3}
            stateS
            {state3 (list :STRING-LITERAL 0)}
            (make-transition-NFA [[stateS state1 \"]
                                  [state1 state1 ALL-ASCII]
                                  [state1 state2 \\]
                                  [state2 state1 ESCAPABLE]
                                  [state1 state3 \"]]))))

;; Character literal
;; \'(\\ESCAPABLE | ALL-ASCII)*\' (\ shown for escaping ")
;; aka \'(.*)\' with escapes inside
(def character-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)
        state3 (gensym :3)
        state4 (gensym :4)]
  (make-NFA (into #{} (concat [\'] ALL-ASCII [\\] ESCAPABLE))
            #{stateS state1 state2 state3 state4}
            stateS
            {state4 (list :CHARACTER-LITERAL 0)}
            (make-transition-NFA [[stateS state1 \']
                                  [state1 state3 ALL-ASCII]
                                  [state1 state2 \\]
                                  [state2 state3 ESCAPABLE]
                                  [state3 state4 \']]))))

;; Identifiers
;; [a-zA-Z][a-zA-Z0-9]*
(def identifier-nfa
  (let [stateS        (gensym :S)
        state1        (gensym :s1)
        state2        (gensym :s2)]
  (make-NFA (into #{} (concat UPPER-ALPHABET LOWER-ALPHABET DIGITS))
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

;; Whitespace
;; [space tab newline]+
(def whitespace-nfa
  (let [stateS        (gensym :S)
        state1        (gensym :s1)]
  (make-NFA (into #{} WHITESPACE)
            #{stateS state1}
            stateS
            {state1  (list :WHITESPACE 0)}
            (make-transition-NFA [[stateS state1 WHITESPACE]
                                  [state1 state1 WHITESPACE]]))))

;; complete nfa from all of the individual RE nfas
(def complete-nfa
  (merge-nfas integer-literal-nfa
              string-literal-nfa
              character-literal-nfa
              identifier-nfa
              whitespace-nfa
              fileFormed-nfa))
