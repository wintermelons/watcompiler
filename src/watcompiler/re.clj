(ns watcompiler.re
  (:require [clojure.set :refer :all]
            [watcompiler.nfa :refer :all]
            [watcompiler.lang :refer :all])
  (:import [watcompiler.nfa NFA]))

(def int-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)
        state3 (gensym :3)
        state4 (gensym :4)]
  (make-NFA (into #{} )
            #{stateS state1 state2 state3 state4}
            stateS
            {state4 (list :INT 1)}
            (make-transition-NFA [[stateS state1  e]
                                  [state1 state2 \i]
                                  [state2 state3 \n]
                                  [state3 state4 \t]]))))

(def integer-literal-nfa
  (let [stateS (gensym :S)
        state1 (gensym :1)
        state2 (gensym :2)]
  (make-NFA (into #{} )
            #{stateS state1 state2}
            stateS
            {state2 (list :INTEGER 1)}
            (make-transition-NFA [[stateS state1 e]
                                  [state1 state2 DIGITS-NONZERO]
                                  [state2 state2 DIGITS]]))))

;; java keywords
;; https://www.student.cs.uwaterloo.ca/~cs444/joos.html

;; operators
;; = assign, == equal, <= lessequal

;; booleans
;; true, false



;; complete nfa from all of the individual RE nfas
(def complete-nfa
  (let [stateS (gensym :S)]
  ;; use default constructor because we no longer have the merged accept-map
  (->NFA (into #{} )
         (union (:states int-nfa) (:states integer-literal-nfa))
         stateS
         (merge (:accept-states int-nfa) (:accept-states integer-literal-nfa))
         (merge (:transitions int-nfa) (:transitions integer-literal-nfa)
                (make-transition-NFA [[stateS (:start int-nfa) e]
                                      [stateS (:start integer-literal-nfa) e]]))
         (merge (:accept-priorities int-nfa) (:accept-priorities integer-literal-nfa)))))


;; merge arbitrary number of nfas
