(ns watcompiler.re
  (:require [clojure.set :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all]
            [clojure.string :as str])
  (:import [watcompiler.nfa NFA]))

;; NFAs for types

;; Integer literal
;; 0 and [1-9][0-9]*
(defn build-integer-literal-nfa
  []
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)]
  (make-NFA (into #{} (concat [\0] DIGITS DIGITS-NONZERO))
            #{stateS state1 state2}
            stateS
            {state1 (list "INTEGER-LITERAL" 0)
             state2 (list "INTEGER-LITERAL" 0)}
            (make-transition-NFA [[stateS state1 \0]
                                  [stateS state2 DIGITS-NONZERO]
                                  [state2 state2 DIGITS]]))))

;; String literal
;; \"(\\[btnfr\"\'\\] | ALL-ASCII)*\" (\ shown for escaping ")
;; aka \"(.*)\" with escapes inside
(defn build-string-literal-nfa
  []
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)
        state3 (gensym :3)]
  (make-NFA (into #{} (concat [\'] ALL-ASCII [\\] ESCAPABLE))
            #{stateS state1 state2 state3}
            stateS
            {state3 (list "STRING-LITERAL" 0)}
            (make-transition-NFA [[stateS state1 \"]
                                  [state1 state1 ALL-ASCII]
                                  [state1 state2 \\]
                                  [state2 state1 ESCAPABLE]
                                  [state1 state3 \"]]))))

;; Character literal
;; \'(\\ESCAPABLE | ALL-ASCII)*\' (\ shown for escaping ")
;; aka \'(.*)\' with escapes inside
(defn build-character-literal-nfa
  []
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)
        state3 (gensym :3)
        state4 (gensym :4)]
  (make-NFA (into #{} (concat [\'] ALL-ASCII [\\] ESCAPABLE))
            #{stateS state1 state2 state3 state4}
            stateS
            {state4 (list "CHARACTER-LITERAL" 0)}
            (make-transition-NFA [[stateS state1 \']
                                  [state1 state3 ALL-ASCII]
                                  [state1 state2 \\]
                                  [state2 state3 ESCAPABLE]
                                  [state3 state4 \']]))))

;; Identifiers
;; [a-zA-Z][a-zA-Z0-9]*
(defn build-identifier-nfa
  []
  (let [stateS        (gensym :S)
        state1        (gensym :s1)
        state2        (gensym :s2)]
  (make-NFA (into #{} (concat UPPER-ALPHABET LOWER-ALPHABET DIGITS))
            #{stateS state1 state2}
            stateS
            {state1  (list "IDENTIFIER" 1)
             state2  (list "IDENTIFIER" 1)}
            (make-transition-NFA [[stateS state1 UPPER-ALPHABET]
                                  [stateS state1 LOWER-ALPHABET]
                                  [state1 state2 UPPER-ALPHABET]
                                  [state1 state2 LOWER-ALPHABET]
                                  [state2 state2 UPPER-ALPHABET]
                                  [state2 state2 LOWER-ALPHABET]
                                  [state2 state2 DIGITS]]))))

;; Whitespace
;; [space tab newline]+
(defn build-whitespace-nfa
  []
  (let [stateS        (gensym :S)
        state1        (gensym :s1)]
  (make-NFA (into #{} WHITESPACE)
            #{stateS state1}
            stateS
            {state1  (list "WHITESPACE" 0)}
            (make-transition-NFA [[stateS state1 WHITESPACE]
                                  [state1 state1 WHITESPACE]]))))

;; Comment
;; //.*
;; /*.**/
(defn build-comment-nfa
  []
  (let [stateS        (gensym :S)
        state1        (gensym :s1)
        state2        (gensym :s2)
        state3        (gensym :s3)
        state4        (gensym :s4)
        state5        (gensym :s5)]
  (make-NFA (into #{} (concat ALL-ASCII [\*] [\/]))
            #{stateS state1 state2 state3 state4 state5}
            stateS
            {state2  (list "COMMENT" 0)
             state5  (list "COMMENT"0)}
            (make-transition-NFA [[stateS state1 \/]
                                  [state1 state2 \/]
                                  [state2 state2 ALL-ASCII]
                                  [state1 state3 \*]
                                  [state3 state3 ALL-ASCII]
                                  [state3 state4 \*]
                                  [state4 state5 \/]]))))

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
    (->NFA (into #{} ) ;; collect the alphabet
               all-states
               stateS
               all-accept-states
               all-transitions
               all-accept-priorities)))

;; Parses a string to form the nfa
(defn string-to-nfa
  [class-type word]
    ;; case on the word, if it is a special token, pass the respective nfa
    (let
      [stateS (gensym :s)
        ;; List of prefixes of word, stored as strings
        states-map (set (rest (reductions str (str) word)))

        ;; Key: substring of word, Value: gensym associated with this state
        gensym-map (into (sorted-map) (for [c states-map]
                                           [c (gensym c)]))
        ;; Key: gensym value, Value: char to get to this state
        states-char-map (into (sorted-map) (for [pair (map list (vals gensym-map) (seq word))]
                                                [(first pair) (second pair)]))

        ;; Accept states
        accept-states-map {(get gensym-map word) (list class-type 0)}

        ;; transitions from previous substring gensym to next substring gensym
        transitions-map (into #{ [stateS (get gensym-map (str (first (seq word))) \a) (first (seq word))] }
                        (for [v (partition 2 1 (vals gensym-map))]
                             [(first v) (second v) (get states-char-map (second v))]))]
      (make-NFA (into #{} (concat (seq word)))
                states-map
                stateS
                accept-states-map
                (make-transition-NFA transitions-map))))

;; Acts as a wrapper to either get the made nfa or form it with string-to-nfa
(defn get-nfa
  [class-type first-token arguments]
  (case first-token
    "<numbers>" (build-integer-literal-nfa)
    "<string>" (build-string-literal-nfa)
    "<character,escape>" (build-character-literal-nfa)
    "<alphanumeric>" (build-identifier-nfa)
    "<spaces,tabs,newlines>" (build-whitespace-nfa)
    "<doubleslash,multilinecomment>" (build-comment-nfa)
    (string-to-nfa class-type first-token)))

;; Takes strings and forms nfas from them and links them into one nfa
(defn form-multiple-nfas
  [& arguments]
  (let
    [stateS (gensym :s)
     class-type (first arguments)
     args (rest arguments)

     ;; Letters of all of the words in arguments
     alphabet (apply concat (for [x (map seq (map char-array arguments))] x))

     ;; Key: string for keyword, Value: NFA for that keyword
     strings-nfas (into (sorted-map) (for [nfa-name args]
                                          [nfa-name (get-nfa class-type nfa-name args)]))

     ;; All of the states in the nfas
     all-states (apply union (map :states (vals strings-nfas)))

     ;; All of the accept states in the nfas
     all-accept-states (apply union (map :accept-states (vals strings-nfas)))

     ;; All of the transitions in the nfas
     merged-transitions (apply merge (map :transitions (vals strings-nfas)))

     ;; Setting epsilon transitions to all of the nfas start states
     all-transitions (merge
                       ;; Merged transitions from the nfas
                       merged-transitions
                       ;; Episilon transition to each nfa
                       (make-transition-NFA (into []
                                            (for [nfa-start (map :start (vals strings-nfas))]
                                                 [stateS nfa-start e]))))
     all-accept-priorities (apply union (map :accept-priorities (vals strings-nfas)))]
    (->NFA (into #{} (concat alphabet))
               all-states
               stateS
               all-accept-states
               all-transitions
               all-accept-priorities)))

;; Reading the file
(def read-file ;; change that notation read-file
  (into []
    (for [line (str/split-lines (slurp "src/watcompiler/Language.txt"))]
         (str/split line #" "))))

(def file-formed-nfa
  (let [nfas (into []
               (for [x read-file]
                 (apply form-multiple-nfas x)))]
  (apply merge-nfas (remove nil? nfas))))

;; complete nfa from all of the individual RE nfas
(def complete-nfa
  (merge-nfas file-formed-nfa))
